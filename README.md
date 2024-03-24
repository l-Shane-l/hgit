[![progress-banner](https://backend.codecrafters.io/progress/git/0612b2fd-2fc8-47aa-9946-746c0bd528fc)](https://app.codecrafters.io/users/codecrafters-bot?r=2qF

# Hgit

- This is a clone of the git command line tool, the intention of this project is educational

## To Run

- stack run

## Notes while building

### Stage 1

- Using lazy byte string might give better performance for larger files, however using it hear results ina file handle error.
- I used deepseq to make it eagerly evaluate as a quick fix but this probably destroys any value in using Lazy ByteString over ByteString.

- I tried to use Monads to make the operations safe especially around the files not being present.

- I also tried to implement this as a library but it was easier in the end to just put all the files into app
