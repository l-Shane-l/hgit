# Hgit

<!--toc:start-->

- [Hgit](#hgit)

  - [To Run](#to-run)
  - [Notes while building](#notes-while-building) - [Stage 1](#stage-1)
  <!--toc:end-->

- This is a clone of the git command line tool,
  the intention of this project is educational

## To Run

- stack run

## Notes while building

### Init

- This was pretty standard, one thing to note was createDirectoryIfMissing
  which is a really useful function.

### CatFile

- Using lazy byte string might give better performance for larger files,
  however using it hear results ina file handle error.
- I used deepseq to make it eagerly evaluate as a quick fix but this probably
  destroys any value in using Lazy ByteString over ByteString.

- I tried to use Monads to make the operations safe especially
  around the files not being present.

- I also tried to implement this as a library but it was easier in
  the end to just put all the files into app

### HashObject

- This one was surprising quick to implement mostly due to the SHA library

- For this task I refactored the code and created a Utils.hs file for
  common functions that really helped.

- I think this task might have been quick due to how I structured
  the code in previous tasks

### LsTree

- This one was the hardest yet, I wrote a lot of code and made early progress
  the structure of the project and the refactoring really helped.
  However when I ran the tests it was giving out badly parsed strings
  I though this was stemming from bad implementation of the Hex types.
  In the end I had to learn how to use Trace to debug it then
  the issue became apparent, I wasnt parsing the string correctly in ParseEnties

- Following passing the tests I refactored the project, I moved functions common to
  multiple functions into Util files in a utils foleder and created an Index to wrap them all.
  The intention here is to make the next steps easier.

- Compared to the http server which I did all in one file the code for this one is growing fast.
  I could probably refactor to reduce the code foot print but I think using a function per command
  makes it a lot easier to reason about and develop.

- For this one I became very familiar with mapM\_, Trace and was able to use an ADT
