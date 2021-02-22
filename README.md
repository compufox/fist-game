# rocket fist
### _ava fox_

Control a fist! Punch the moon!

## Building

install a lisp ([roswell](https://github.com/roswell/roswell))

### Clone the repo and dependencies

```shell
$ mkdir ~/common-lisp
$ git clone https://github.com/compufox/fist-game ~/common-lisp/fist
$ git clone https://github.com/compufox/trivial-gamekit-timeline ~/common-lisp/
$ git clone https://github.com/compufox/trivial-gamekit-simple-menus ~/common-lisp/
```

### Set up an extra quicklisp distribution

```
$ cd ~/common-lisp/fist
$ ros run
* (ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")
```

### Build

```lisp
* (ql:quickload '(:trivial-gamekit/distribution :fist))
* (gamekit.distribution:deliver :fist 'fist::game :build-directory "build/")
```

Once the build process is complete, you should have a `build` directory in the folder with the source code for the game.
If everything went well (no errors occured) you should have a folder, a manifest file, and a zip file. the zip file contains the same thing as the folder. if youre just looking to run the built executable, open the `build/fist` folder and run the executable.

happy punching~!

## License

Code: GPLv3

Assets (besides spaceship.wav): CC BY-NC 4.0

spaceship.wav made by [mrpoly on opengameart](https://opengameart.org/content/space-music)
