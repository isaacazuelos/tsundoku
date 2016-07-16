# Tsundoku User Manual

Tsundoku tracks your piles of unread books.

## Introduction

[Tsundoku][wiki] is a Japanese word that literally means reading pile. The
idea is that you buy books you want to read and and leave them in a pile.
This app helps keep your pile manageable.

[wiki]: https://en.wikipedia.org/wiki/Tsundoku

## Table of Contents

- [Introduction](#introduction)
- [Table of Contents](#table-of-contest)
- [Examples](#examples)
    - [First run](#first-run)
    - [Adding and starting](#adding-and-starting)
- [Common Issues](#common-issues)
    - Fixing incorrect book details
    - Authors with unusual names
    - Books with multiple titles
    - Books with the same titles
    - Multiple versions of the same book
- [Commands](#commands)
    - [Working with the app](#working-with-the-app)
        - Get help with [`help`](#help)
        - Create a new pile with [`init`](#init)
        - Get path information with [`path`](#path)
    - [Working with your pile](#working-with-your-pile)
        - Browse your books with [`list`](#list)
        - Add new books with [`add`](#add)
        - Remove books with sadness and [`remove`](#remove)
    - [Working with books](#Working-with-books)
        - Find out more about a book with [`details`](#details)
        - Mark a book you've started with [`start`](#start)
        - Finish a book with [`finish`](#finish)
        - Ditch books that aren't worth finishing with [`abandon`](#abandon)
        - Tag your books with [`tag`](#tag)
- [Thanks](#thanks)
- [License](#license)

## Examples

### First run

First we need to clone it from github.

``` sh
example:~$ git clone https://github.com/isaacazuelos/tsundoku
example:~$ cd ./tsundoku
```

Now we need to install it with [`stack`][]. Make sure that `stack` puts things
in your `$PATH`.

``` sh
example:tsundoku$ stack init
example:tsundoku$ stack install
```

Before we can use it, we need to `init` our Tsundoku pile.

``` sh
example:tsundoku$ doku init
```

### Adding and starting

Suppose we wanted to add [_A Game of Thrones_][GoT] to our pile. We would do
this with the `add` command.

[GoT]: https://en.wikipedia.org/wiki/A_Game_of_Thrones

``` sh
example:tsundoku$ doku add "A Game of Thrones" --firstname "George R.R." --lastname "Martin" --published 1996
```

We need to mark it as started too. Suppose we started it on February 29th,
2016.

``` sh
example:tsundoku$ doku start "A Game of Thrones" --started 2010-04-29
```

We can confirm it's added with `details` too.

``` sh
example:tsundoku$ doku details "A Game of Thrones"
George R.R. Martin - A Game of Thrones (1996) started 2010-04-29
```

Suppose we wanted to type less, and mark that we have the audiobook version
and that it's a work of fiction using tags.

``` sh
example:tsundoku$ doku add "A Game of Thrones" -f "George R.R." -l Martin -p 1996 -t audiobook -t fiction
```

## Common Issues

Here are some issues that will probably come up a lot.

### Fixing incorrect book details

Don't fix it, just do a `details`, `remove` and `add`, fixing the error in
the `add`.

If you're technically savvy enough, you can try to edit your pile's file,
which you can find with the [`path`](#path) command.

### Authors with atypical names

Some authors have more than one last name, or more than one first name.
Figuring out where to find things in a bookstore is hard enough for humans.
That's why `doku` asks for the last names and first names each. You can use
quotes around the names to make sure they're parsed correctly.

For example, you might do something like the following:

``` sh
doku add "The Philosophy of Natural Magic" \
  --firstname "de Laurence" \
  --lastname "L. W." \
  --published 1868
```

### Books with multiple titles

Just pick one of them. Honestly, it doesn't matter. You know what you mean.

It's either Albert Camus's _The Outsider_ or _The Stranger_, either way you
know what you're talking about.

### Books with the same titles

Just pick one of them, and commit to never reading the other. There are
plenty of other books worth reading.

Or you could, you know, _lie_ and put some other differentiating indicator in
your titles.

### Multiple versions of the same book

Is this really what you want to be doing with your life?

### Books that differ only by secondary authors with the same titles

Is this really what you want _me_ to be doing with my life?

## Commands

The `doku` command responds a bunch of _commands_.

### Working with the app

These commands are for working with the `doku` app itself, i.e. working with
things other than the books in the pile.

#### `help`

> Only the switch version with `--help` is currently implemented.

Usage:
``` sh
doku --help
doku help
```

The `help` command prints a fairly brief help message and a pile of other
command descriptions.

#### `init`

Usage:
``` sh
doku init [--reset]
```

The `init` command is used to initialize a `doku` pile. Trying to initialize
again if there's an existing pile will only let you know that a pile exists.

If you set the `--reset` switch, it'll wipe your pile.

#### `path`

Usage:
``` sh
doku path
```

The `path` command prints the absolute path of the current Tsundoku file.

### Working with your pile

#### `list`

Usage:
``` sh
doku list \
  [-l|--sort] \
  [-a|--alltags] \
  [-s|--status STATUS] \
  [-t|--tag TAG]
```

Dumps out a nice, human-readable list of all books. There are flags you can
set to limit the output in various ways.

By default, your pile is printed with the most recent books on top. You can
flip the `--sort` switch to have it sort the books in the usual way, i.e. by
author last name, first name, then title.

You can limit the search to books of a particular read status by setting
`--status` argument a value of `unread`, `started`, `finished` or
`abandoned`. You can also just use the first letter of one of these.

You can use `--tag` to filter the results to books with that tag.

#### `add`

Usage
``` sh
doku add TITLE \
	[-f|--firstname NAME] \
	[-l|--lastname NAME] \
	[-o|--otherauthors NAMES] \
	[-p|--published YYYY] \
	[-t|--tag TAG]
```

Books are added as unread. You can set the status after with the
[`unread`](#unread), [`finish`](#finish), [`start`](#start), or
[`abandon`](#abandon) commands.

You can't add a book if there's already a book with that title. Yes, there
are real pairs of books that share titles.

Setting the publication date with `--published` takes a series of digits as a
year.

#### `remove`

Usage

``` sh
doku remove TITLE
```

Remove a book from your pile forever.

### Working with books

#### A note about dates

When a date needs to be specified, use the `yyyy-mm-dd` date format. If you
want to specifically not use a date, say if you're trying to remove an
incorrect date, you can use `none` as the date as well.

#### `details`

Usage

``` sh
doku details TITLE
```

Shows you everything known about a book in your pile.

#### `start`

Usage

``` sh
doku start TITLE [-s|--started DATE]
```

Mark books as started. By default the current date is recorded, but you can
set one with `--started`. Trying to start a started book will update the start
date.

#### `finish`

Usage

``` sh
doku finish TITLE [-s|--started DATE] [-f|--finished DATE]
```

Mark books as finished. Specified dates are used to override existing dates,
and dates carry over from the existing status -- using `finish` on a started
book will keep the existing `started` date if none was specified.

#### `abandon`

Usage

``` sh
doku abandon TITLE \
	[-s|--started DATE] \
	[-a|--abandoned DATE] \
	[-p|--place PLACE]
```

Mark books as abandoned. By default the current date is recorded, but you can
set one with `--date`. You can also record how far you made it with `--page`.

The `PAGE` given is stored verbatim, so the following will work.

``` sh
doku abandon "A very bad book" --place "Chapter 3"
```

#### `tag`

Usage:

``` sh
doku tag TITLE TAG [-r|--remove]
```

You can add tags to books in your pile.

If `--remove` is set, then the tag specified will be removed.

## Thanks

I'll thank any contributors here! That could be you!

## License

It's [MIT] Licensed. See the included `LICENSE` file in the project root.

[mit]: https://opensource.org/licenses/MIT
