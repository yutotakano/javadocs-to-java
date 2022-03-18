#!/usr/bin/env stack --resolver lts-18.24 --install-ghc runghc --package scalpel --package neat-interpolation --package tagsoup --package unordered-containers
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent.MVar
import Data.ByteString.Lazy qualified as BL
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.HTML.Scalpel
import Text.Printf
import System.Directory
import System.Environment (getArgs)

-- | Helper to comment some text. No word-wrapping.
commentify :: Int -> T.Text -> T.Text
commentify spaces text = T.replicate spaces " " <> "/**\n" <> (T.unlines $ commentifyLines $ T.lines text) <> T.replicate spaces " " <> " */"
  where
    commentifyLines :: [T.Text] -> [T.Text]
    commentifyLines = map (\x -> T.replicate spaces " " <> " * " <> T.strip x)

-- | Typeclass for code-rendering. Output strict Text.
class Render a where
    render :: a -> T.Text

data JavaPackage = JavaPackage
    { packageName :: T.Text
    , packageDescription :: T.Text
    , packageInterfaces :: [JavaInterface]
    , packageClasses :: [JavaClass]
    , packageEnums :: [JavaEnum]
    } deriving (Show, Eq)

instance Render JavaPackage where
    render JavaPackage{..} = commentDescription <> "\npackage " <> packageName <> ";"
      where
        commentDescription :: T.Text
        commentDescription = commentify 0 packageDescription

data JavaInterface = JavaInterface
    { interfaceName :: T.Text
    , interfaceDescription :: T.Text
    , interfaceMethods :: [JavaMethod]
    } deriving (Show, Eq)

instance Render JavaInterface where
    render JavaInterface{..} = T.pack $ printf "%s\npublic interface %s {\n%s\n}\n" commentDescription interfaceName $ render interfaceMethods
      where
        commentDescription :: T.Text
        commentDescription = commentify 0 interfaceDescription

data JavaClass = JavaClass
    { className :: T.Text
    , classSignature :: T.Text
    , classDescription :: T.Text
    , classConstructors :: [JavaMethod]
    , classMethods :: [JavaMethod]
    } deriving (Show, Eq)

instance Render JavaClass where
    render JavaClass{..} = T.pack $ printf "%s\n%s {\n%s\n}\n" commentDescription classSignature $ render (classConstructors <> classMethods)
      where
        commentDescription :: T.Text
        commentDescription = commentify 0 classDescription

data JavaMethod = JavaMethod
    { methodName :: T.Text
    , methodDescription :: T.Text
    , methodModifiers :: T.Text
    , methodReturnType :: Maybe T.Text
    , methodArgumentTypes :: [(T.Text, T.Text)]
    , methodJavaDocTags :: [(T.Text, Maybe T.Text)]
    } deriving (Show, Eq)

instance Render JavaMethod where
    render JavaMethod{..} = T.pack $ printf "%s\n    %s%s(%s) {}\n" comment mods methodName args
      where
        comment :: T.Text
        comment
            | methodDescription == "" && null methodJavaDocTags = ""
            | methodDescription == "" = commentify 4 $ T.intercalate "\n" (map renderDocTag methodJavaDocTags)
            | null methodJavaDocTags = commentify 4 $ methodDescription <> "\n"
            | otherwise = commentify 4 $ methodDescription <> "\n\n" <> T.intercalate "\n" (map renderDocTag methodJavaDocTags)

        mods :: T.Text
        mods
            | methodModifiers == "" && methodReturnType == Nothing = ""
            | methodModifiers == "" = fromJust methodReturnType <> " "
            | methodReturnType == Nothing = methodModifiers <> " "
            | otherwise = methodModifiers <> " " <> fromJust methodReturnType <> " "

        renderDocTag :: (T.Text, Maybe T.Text) -> T.Text
        renderDocTag ("Returns:", Just val) = "@returns " <> val
        renderDocTag ("Parameters:", Just val) = "@param " <> T.unwords (T.splitOn " - " val)
        renderDocTag ("Overrides:", Nothing) = "@overrides"
        renderDocTag ("inheritDoc", Nothing) = "{@inheritDoc}"
        renderDocTag (a, Nothing) = "@" <> a
        renderDocTag (a, Just b) = "@" <> a <> " " <> b

        args :: T.Text
        args = T.intercalate ", " $ map (\(n, t) -> t <> " " <> n) methodArgumentTypes

instance Render [JavaMethod] where
    render jms = T.intercalate "\n" $ map render jms

data JavaEnum = JavaEnum
    { enumName :: T.Text
    , enumValues :: [T.Text]
    } deriving (Show, Eq)

instance Render JavaEnum where
    render JavaEnum{..} = T.pack $ printf "public enum %s {\n    %s\n}" enumName enumConsts
      where
        enumConsts :: T.Text
        enumConsts = T.intercalate ", " enumValues

main :: IO ()
main = do
    args <- getArgs
    res <- case args of
        ["online", url] -> pure $ InternetResource url
        ["local", dirname] -> pure $ LocalResource dirname
        _ -> error $
            "Supply either an online JavaDoc base host or a local path:\n" <>
            "\n" <>
            "  ./converter.hs online https://javadoc.page.without.trailing.slash\n" <>
            "  ./converter.hs local the_dirname_without_trailing_slash\n" <>
            "\n" <>
            "All files will be requested by appending e.g. /index.html to the given base url/path.\n" <>
            "The output will be a \"src\" folder. Import statements are not added automatically, so\n" <>
            "please use something like IntelliJ to go through and fix any missing imports.\n\n"
    exists <- doesDirectoryExist "src"
    when exists $ error "`src` directory already exists!\nThis program will not overwrite it, so please delete/move the existing folder.\n\n"
    createDirectory "src"
    packageNames <- getPackageNames res
    packages <- catMaybes <$> mapM (getPackage res) packageNames
    forM_ packages $ \pac -> do
        createDirectoryIfMissing False $ T.unpack $ "src" <> "/" <> packageName pac
        TIO.writeFile (T.unpack $ "src" <> "/" <> packageName pac <> "/package-info.java") $
            render pac
        forM_ (packageInterfaces pac) $ \int -> do
            TIO.writeFile (T.unpack $ "src" <> "/" <> packageName pac <> "/" <> interfaceName int <> ".java") $
                "package " <> packageName pac <> ";\n\n" <> render int
        forM_ (packageClasses pac) $ \cls -> do
            TIO.writeFile (T.unpack $ "src" <> "/" <> packageName pac <> "/" <> className cls <> ".java") $
                "package " <> packageName pac <> ";\n\n" <> render cls
        forM_ (packageEnums pac) $ \enu -> do
            TIO.writeFile (T.unpack $ "src" <> "/" <> packageName pac <> "/" <> enumName enu <> ".java") $
                "package " <> packageName pac <> ";\n\n" <> render enu

data Resource = InternetResource String | LocalResource String

scrapeRes :: Resource -> String -> ScraperT T.Text IO a -> IO (Maybe a)
scrapeRes (InternetResource baseUrl) page scraper =
    join $ scrapeT scraper `liftM` fetchTagsWithConfig def (baseUrl <> "/" <> page)
scrapeRes (LocalResource dir) page scraper = do
    contents <- TIO.readFile $ dir <> "/" <> page
    scrapeStringLikeT contents scraper

getPackageNames :: Resource -> IO ([T.Text])
getPackageNames res =
    fmap (maybe [] id) $ scrapeRes res "index.html" $
        chroots ("tbody" // "tr" // "th") $
            text "a"

data FileTypes = JavaInterfaces [JavaInterface] | JavaClasses [JavaClass] | JavaEnums [JavaEnum] deriving (Show, Eq)

getPackage :: Resource -> T.Text -> IO (Maybe JavaPackage)
getPackage res pacName =
    scrapeRes res (T.unpack pacName <> "/package-summary.html") $ do
        description <- text $ "section" @: [hasClass "packageDescription"] //
            "div" @: [hasClass "block"]
        files <- chroots ("section" @: [hasClass "summary"] // "div" @: [hasClass "typeSummary"]) $ do
            tableTitle <- text $ "caption" // "span"
            tableRows <- attrs "href" $ "tbody" // "tr" // "th" // "a"
            case head (T.words tableTitle) of
                "Interface" -> do
                    is <- catMaybes <$> mapM (liftIO . getInterface res pacName) tableRows
                    pure $ JavaInterfaces is
                "Class" -> do
                    cs <- catMaybes <$> mapM (liftIO . getClass res pacName) tableRows
                    pure $ JavaClasses cs
                "Enum" -> do
                    es <- catMaybes <$> mapM (liftIO . getEnum res pacName) tableRows
                    pure $ JavaEnums es
        let putInPackage (JavaInterfaces is) p = p { packageInterfaces = is }
            putInPackage (JavaClasses cs) p = p { packageClasses = cs }
            putInPackage (JavaEnums es) p = p { packageEnums = es }
        pure $ foldr putInPackage (JavaPackage pacName description [] [] []) files

getInterface :: Resource -> T.Text -> T.Text -> IO (Maybe JavaInterface)
getInterface res pacName intPage =
    scrapeRes res (T.unpack pacName <> "/" <> T.unpack intPage) $ do
        name <- text $ "span" @: [hasClass "typeNameLabel"]
        description <- text $ "section" @: [hasClass "description"] //
            "div" @: [hasClass "block"]
        methods <- chroots ("section" @: [hasClass "methodDetails"] // "li" @: [hasClass "blockList"]) scrapeMethod
        pure $ JavaInterface name description methods

getClass :: Resource -> T.Text -> T.Text -> IO (Maybe JavaClass)
getClass res pacName clsPage =
    scrapeRes res (T.unpack pacName <> "/" <> T.unpack clsPage) $ do
        (clsSig, description) <- chroot ("section" @: [hasClass "description"]) $ do
            sig <- texts "pre"
            desc <- (text $ "div" @: [hasClass "block"]) <|> pure ""
            pure (head sig, desc)
        name <- text $ "span" @: [hasClass "typeNameLabel"]
        cons <- chroots ("section" @: [hasClass "constructorDetails"] // "li" @: [hasClass "blockList"]) scrapeMethod
        methods <- chroots ("section" @: [hasClass "methodDetails"] // "li" @: [hasClass "blockList"]) scrapeMethod

        pure $ JavaClass name clsSig description cons methods

getEnum :: Resource -> T.Text -> T.Text -> IO (Maybe JavaEnum)
getEnum res pacName enuPage =
    scrapeRes res (T.unpack pacName <> "/" <> T.unpack enuPage) $ do
        name <- text $ "span" @: [hasClass "typeNameLabel"]
        values <- texts $ "section" @: [hasClass "constantsSummary"] // "tbody" // "tr" // "th" // "span" @: [hasClass "memberNameLink"] // "a"

        pure $ JavaEnum name values

scrapeMethod :: ScraperT T.Text IO JavaMethod
scrapeMethod = chroot ("section" @: [hasClass "detail"]) $ do
    name <- text "h3"
    modifiers <- (text $ "div" @: [hasClass "memberSignature"] // "span" @: [hasClass "modifiers"]) <|> pure ""
    returnType <- Just <$> (text $ "div" @: [hasClass "memberSignature"] // "span" @: [hasClass "returnType"]) <|> pure Nothing
    description <- (text $ "div" @: [hasClass "block"]) <|> pure ""
    argumentsTypesStr <- (texts $ "div" @: [hasClass "memberSignature"] // "span" @: [hasClass "arguments"]) <|> pure []
    let argumentTypes = if null argumentsTypesStr then [] else
            map ((\[a, b] -> (b, T.replace "\x200b" " " a)) . T.split (== '\160') . T.takeWhile (/= ')')) $ T.splitOn ",\r\n" $ head argumentsTypesStr

    params <- (<|> pure []) $ chroot "dl" $ inSerial $ do
        many $ do
            paramName <- seekNext $ text $ "dt" // textSelector `atDepth` 2
            case paramName of
                "Verifies that:" -> untilNext (matches "dt") $ seekNext $ do
                    t <- texts "li"
                    pure $ map (\x -> ("verifies.that", Just x)) t
                "Specified by:" -> pure []
                _ -> untilNext (matches "dt") $ many $ seekNext $ do
                    -- since @texts@ can succeed with empty input, make sure we get at least one element
                    t <- texts "dd"
                    guard $ not $ null t
                    pure (paramName, Just $ head t)

    let (additionalParam1, inheritRemovedDesc) = case T.take 33 description of
            "Description copied from interface" -> ([("inheritDoc", Nothing)], "")
            _ -> ([], description)

    pure $ JavaMethod name inheritRemovedDesc modifiers returnType argumentTypes (join params <> additionalParam1)
