# Reasoning

This document records the justifications for technical decisions made.

## Genres

I don't really like classifying books beyond fiction–nonfiction.

Tags can do the same thing better anyway.

## Identifying books by title alone

I tried writing out the manual for using titles and optionally taking authors
to differentiate books with different authors but the same titles. It was
messy and didn't seem to be useful enough to bother with.

## Storage

### Persistence

I think JSON is the easiest way to go.

While a proper database is the right call for something like this in the
large, I can't image someone being so well read as to have more than a few
thousand entries. Something like sqlite would be a good call if I had the
patience.

I also might be adding fields, or want more than one language to work with the
files, and JSON makes that a little easier.

### Location

Right now, things are in a hidden file in `$HOME` and there's at most one.

I want to model a pile after a git repository, mostly because its a
model the average user will be familiar with.

Here are a few other possibilities, and why I don't like them.

#### Hidden folder in `$HOME`

I think it makes sense to have more than one library. That's pretty much it
for this one. That's why this is the current solution, it's easy and it works.

#### Explicit path for every verb

This would get really repetitive. While you could set an alias, I don't think
expecting people to make the commands better is really the right solutions.
