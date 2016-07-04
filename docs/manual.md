# Tsundoku User Manual

Tsundoku tracks your piles of unread books.

## Warnings

This version of the manual has all the planned commands. Not all of them are implemented, yet.

Some of the syntax for the usage might change too.

## Introduction

[Tsundoku][wiki] is a Japanese word that literally means reading pile. The
idea is that you buy books you want to read and and leave them in a pile. This
app helps keep your pile manageable.

[wiki]: https://en.wikipedia.org/wiki/Tsundoku

## Table of Contents

- [Warnings](#warnings)
- [Introduction](#introduction)
- [Table of Contents](#table-of-contest)
- [Examples](#examples)
- [Common Issues](#common-issues)
    - Fixing incorrect book details
    - Authors with unusual names
    - Books with multiple titles
    - Books with the same titles
    - Multiple versions of the same book
- [Verbs](#verbs)
    - [Working with the app](#working-with-the-app)
        - Get help with [`help`](#help)
        - Create a new pile with [`init`](#init)
        - Get path information with [`path`](#path)
    - [Working with your pile](#working-with-your-pile)
        - Browse your books with [`list`](#list)
        - Add new books with [`add`](#add)
        - Remove books with sadness and [`remove`](#delete)
    - [Working with books](#Working-with-books)
        - Find out more about a book with [`details`](#details)
        - Mark a book you've started with [`start`](#start)
        - Finish a book with [`finish`](#finish)
        - Ditch crap books with [`abandon`](#abandon)
        - Tag your books with [`tag`](#tag)
        - Fix errors with [`fix`](#fix)
- [Thanks](#thanks)
- [License](#license)

## Examples

There will be some, once the app exists. Why write some examples when `script` will record them.

## Common Issues

Here are some issues that will probably come up a lot.

### Fixing incorrect book details

Don't fix it, just do a `details`, `delete` and `add`, fixing the error in the `add`. This looks like a lot of work, but the adding a verb for correcting errors introduced way more complexity. If you're technically savvy enough, you can try to edit your pile's file, which you can find with the [`path`](#path) verb.

### Authors with atypical names

Some authors have more than one last name, or more than one first name.
Figuring out where to find things in a bookstore is hard enough for humans.
This is why `tsundoku` asks for the last names and first names each. You can
use quotes around the names to make sure they're parsed correctly.

For example, you might do something like the following:

``` sh
$ tsundoku add "The Philosophy of Natural Magic" \
  --lastnames  "de Laurence" \
  --firstnames "L. W." \
  --published 1868
```

### Books with multiple titles

Just pick one of them. Honestly, it doesn't matter. You know what you mean.
It's either Albert Camus's _The Outsider_ or _The Stranger_, either way you
know what you're talking about.

### Books with the same titles

Just pick one of them, and commit to never reading the other. There are plenty
of other books worth reading.

Or you could, you know, _lie_ and put some other differentiating indicator in
your titles.

### Multiple versions of the same book

Is this really what you want to be doing with your life?

### Books that differ only by secondary authors with the same titles

Is this really what you want _me_ to be doing with my life?

## Verbs

The `tsundoku` command responds a bunch of _verbs_.

### Working with the app

These verbs are for working with the `tsundoku` app itself.

#### `help`

Usage

``` sh
$ tsundoku --help
$ tsundoku help
```

The `help` verb prints a fairly brief help message and a pile of verbs. It's probably what pointed you to this manual.

#### `init`

Usage

``` sh
$ tsundoku init [--reset]
```

The `init` verb is used to initialize a `tsundoku` pile. Trying to initialize again if there's an existing pile won't do anything.

If you set the `--reset` flag, it'll wipe your pile.

#### `path`

Usage

``` sh
$ tsundoku path
```

The `path` verb prints the path of the current Tsundoku directory. It prints nothing if there isn't one closer to root than the current directory.

### Working with your pile

#### `list`

Usage

``` sh
$ tsundoku list \
  [-a|--alphabetical] \
  [-s|--status STATUS] \
  [-t|--tag TAG]
```

Dumps out a nice, human-readable list of all books. There are flags you can set to limit the output in various ways.

By default, your pile is printed with the most recent books on top. You can set the `--alphabetical` flag to have it sort the books in the usual way, i.e. by author last name, first name, then title.

You can limit the search to books of a particular read status by setting `--status` argument a value of `unread`, `finished` or `abandoned`. You can also just use the first letter.

Where `TAG` is any tags to the results to. If you need to filter to multiple tags, try using [`grep`](https://www.gnu.org/software/grep/manual/grep.html)

#### `add`

There are two ways to use the `add` command. You can specify each necessary field of the book by name. Any missing fields will cause a failure. You can also set the `--interactive` flag, which will ask a series of questions to gather the missing fields.

Usage

``` sh
$ tsundoku add TITLE\
  [-i|--interactive] \
  [-l|--lastnames LASTNAMEs] \
  [-f|--firstnames firstNAMES] \
  [-o|--otherauthors OTHERAUTHORS] \
  [-p|--published YEAR]
```

Books are added as unread and without tags. If that's incorrect, you can fix book with [`fix`](#fix).

You can't add a book if there's already a book with that title. Yes, there are real pairs of books that share titles. Trust me, things are simpler if you just pick one.

#### `remove`

Usage

``` sh
$ tsundoku delete TITLE
```

Remove a book from your pile forever.

### Working with books

#### `details`

Usage

``` sh
$ tsundoku details TITLE
```

Shows you everything it known about a book in your pile. This shows not just information you've added about the book, but also where you are in reading it.

#### `start`

Usage

``` sh
$ tsundoku start TITLE [-d|--date DATE]
```

Mark books as started. By default the current date is recorded, but you can set one with `--date`.

#### `finish`

Usage

``` sh
$ tsundoku finish TITLE [-d|--date DATE]
```

Mark books as finished. By default the current date is recorded, but you can set one with `--date`.

#### `abandon`

Usage

``` sh
$ tsundoku abandon TITLE [-d|--date DATE] [-p|--page PAGE]
```

Mark books as abandoned. By default the current date is recorded, but you can set one with `--date`. You can also record how far you made it with `--page`.

The `PAGE` given is stored verbatim, so the following will work.

``` sh
$ tsundoku abandon "A very bad book" --page "Chapter 3"
```

#### `tag`

Usage:

``` sh
$ tsundoku tag TITLE [-r|--remove] TAG
```

You can add tags to books in your pile.

If `--remove` is set, then the tag specified will be removed.

## Thanks

I'll thank any contributors here! That could be you!

## License

It's [MIT] Licensed. See the included `LICENSE` file in the project root.

[mit]: https://opensource.org/licenses/MIT
