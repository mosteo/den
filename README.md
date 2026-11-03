## Den (Link-aware Directory Enumeration)

Yet another library to enumerate directories, this time with explicit emphasis
on dealing with soft links.

## Platform notes

Den aims to behave the same on Linux, Windows, macOS, FreeBSD and OpenBSD. CI
runs on Linux, Windows and macOS. The BSDs use the same POSIX code as the other
Unix-like systems, but they remain untested.

### Names

Names are opaque to Den, and they are never normalized. Two names that look the
same can be different files on Linux, for instance when using composed and
decomposed accents. On Unix-like systems a name is whatever bytes the filesystem
stores, which doesn't have to be valid UTF-8.

On Windows, Den's strings are always UTF-8, regardless of `GNAT_CODE_PAGE`. They
can also hold the unpaired surrogates that NTFS allows in names, which makes
them WTF-8, so any name can be listed and used. Such names don't work with the
GNAT runtime, nor do non-ASCII names in a program that sets
`GNAT_CODE_PAGE=CP_ACP`.

macOS may store a name differently from how it was given, so compare the result
of `Canonical` and not the names as given.

### Links

`Softlink` means a symlink, or an NTFS junction on Windows. Deleting a tree
removes the links in it and leaves their targets alone, and copying keeps them
as they are, including what kind of link they are. Other Windows items that
stand in for another one are `Special`, and they are never entered. OneDrive
placeholders and deduplicated files count as regular files or folders.

Creating symlinks on Windows needs Developer Mode or elevation, whereas junctions
need neither.

### Paths on Windows

- `Scrub` leaves `/` alone, as git produces such paths, but it remains a separator
  for the OS and for subprograms like `Parts`, `Parent` or `Relative`. Paths that
  are rebuilt from their parts use `\`. A verbatim `\\?\` path only has `\`.
- `\foo`, `C:foo` and `C:` depend on the current drive or directory, so they are
  neither absolute nor plainly relative, and they can't be appended to a path.
  `Absolute` resolves them.
- The root of a network path is `\\server\share\`.
- `Relative` doesn't compare case, other than in drive letters and server names.
  Its result is valid in any case, but it may be longer than needed.

### Limits

The longest path that the OS accepts in a call is:

| OS                      | Length                                |
|-------------------------|---------------------------------------|
| Linux                   | 4096 bytes                            |
| macOS, FreeBSD, OpenBSD | 1024 bytes                            |
| Windows                 | About 32767 characters, handled by Den |

A tree on a Unix-like system can go deeper than that by descending into it, and
Den cannot go through such a tree. Operations that reach that point raise
`Use_Error` instead of taking the path as missing, although `Kind` does return
`Nothing` for it.

Trees are walked by path. If someone else can write into a tree while
`Delete_Tree` goes through it, they could replace a folder with a link between
the check and the deletion, and it would be followed. Don't run it with more
privileges than those of everyone who can write to that tree.
