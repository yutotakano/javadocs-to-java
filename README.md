# javadocs-to-java

Makeshift script to generate Java code from rendered JavaDoc HTML files.

Created for my own use, please pardon my lack of comments.

## To run

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/)

### Command-line

- `./converter.hs online https://homepages.inf.ed.ac.uk/...`
- `./converter.hs local javadoc_dir`

This will create:
- a `src` directory
- individual package directories within it
- all classes, interfaces, and enums within each package, complete with JavaDoc tag annotated comments & type signatures

This will not create:
- import statements, as these are a great pain - please do this with IntelliJ manually
- compliable code - all methods are generated as empty `{}`. This will not compile because there's no return statement.
- valid interfaces - since the same generator is used as classes, the interface will have `{}` as method bodies. You'll need to remove those and replace with a single semicolon after the conversion.
