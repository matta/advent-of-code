;;;; Advent of Code 2020 Day 20.
;;;;
;;;; I found this problem surprisingly difficult.  Primary lesson learned:
;;;; introduce and use types early.  In this case, we're dealing with
;;;; various operations on bitmaps and I initially wrote a lot of code
;;;; dealing with (x y) and (height width) args, using multi-dimensional
;;;; arrays as my "image" type.  I haven't done much with defstruct and
;;;; defclass because I wanted to see how far lists and arrays could take
;;;; me.  This approach was particularly error prone on this problem, and I
;;;; lost a lot of time looking for bugs that amounted to transposition
;;;; errors and other typos.  As I lifted the level of abstraction up many
;;;; of these bugs turned into compile time or run time errors, or were
;;;; easier to spot when reading code.
;;;;
;;;; So, the end result is that this code is over designed for an Advent of
;;;; Code solution.  I was exploring CommonLisp bit vectors/arrays, custom
;;;; type printerers (PRINT-OBJECT), bit vector read syntax, etc.
;;;;
;;;; Note to self: my first solution used a multi-dimensional bit array.
;;;; When I switched to one dimensional bit arrays the compiler (SBCL)
;;;; generated code that ran roughly 10-20 times faster.

(defpackage #:ma-aoc-2020/day20
  (:use #:common-lisp))
(in-package #:ma-aoc-2020/day20)

(defparameter *sea-monster*
  '("..................#."
    "#....##....##....###"
    ".#..#..#..#..#..#..."))

(defparameter *example-input*
  "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...")

(defparameter *input*
  "Tile 3923:
...##.....
.##.#..#.#
###....#..
...#...#.#
....#.#...
..........
.....#.###
.##.#..#..
..##..#...
..#.#.##..

Tile 1229:
###.....#.
..########
...##...#.
...#..#...
....#.....
..#.....#.
#......#..
#.....##.#
##.#....##
.##.......

Tile 1801:
..###...#.
.....#....
.....#...#
#...##...#
#........#
.....#....
#.####..#.
.....#....
.#.#......
..#..###..

Tile 3643:
##..#.#...
.......#.#
#.#..#.###
.....#.#..
.......#..
........#.
..#..#.#.#
#........#
#...#..###
..#.......

Tile 2141:
...###.##.
#.##..#...
##.#.....#
#..#.....#
#.#..###.#
.......###
#...##.###
#......#..
#...#....#
.##..##.#.

Tile 1289:
....#...#.
###.....##
.##.......
....#..##.
.#.......#
...#....#.
...#.#.##.
.##..##..#
.........#
.##.####..

Tile 3373:
#####.####
..###.##..
#..#...##.
.#....#..#
.......#.#
...#..#.##
#.#..##...
####....#.
....##.#..
.###.####.

Tile 1399:
..#.#.#...
#..#...#.#
...####.##
.....#....
........##
.......#..
.##...#...
#..#......
.....#..#.
#...#..###

Tile 3259:
###...#.##
..........
..........
...#..##..
#....###.#
#....##...
#.##.#.###
....##...#
.........#
#..##.#..#

Tile 3469:
#..#####.#
#..#.....#
......#...
..#.##.##.
#..##....#
......##..
#.#.....#.
.##.......
#.#.......
#..#...###

Tile 2143:
##.#.###.#
....#..##.
#.........
#....##...
#.......#.
.###......
.#....#..#
#.#......#
.....#..#.
##.####.#.

Tile 2663:
....#####.
.#..#.....
.#......##
.#........
#....##..#
#..#......
##..##...#
#.....#.##
#...#...##
#....#...#

Tile 3067:
#.#.##..#.
..........
#..#......
#...#....#
#.##.#....
##.......#
#....#....
#......#..
#....#.#.#
##.#......

Tile 2903:
##...#.#.#
..#....#.#
#.##.#.###
#.#..#...#
#......#.#
........#.
#.#.#...#.
##.#...##.
...#..#.##
####.##.##

Tile 1471:
###.##..#.
#....#.#..
..##.#...#
........#.
#.....##..
.#........
#..#.....#
#.....##.#
.#...###..
..##..#.##

Tile 3251:
.####..#..
.#.......#
#.##.#....
.......###
.#..#....#
....#....#
#...####..
##..#.####
.......###
....#....#

Tile 1583:
...#..###.
.....###..
#..##...##
######..#.
.....###.#
#..#.#....
.#..###..#
..#...##..
#.#.....##
###..##..#

Tile 1747:
##.#..#...
......#.##
....#....#
.....#...#
#..#.#..##
#...#.....
#......#.#
#....##.#.
....#...#.
##....##..

Tile 1259:
#..#.....#
...##.#.##
.#...#.#..
.....##.#.
#......#..
#........#
........#.
.##.....##
###.#..##.
###..#....

Tile 1993:
#..#......
#..#..##.#
##.##..###
.....##...
.#.#..#..#
......#..#
....#.....
....#..#..
#.#####..#
..#..####.

Tile 3391:
#..####...
###....##.
##.....###
..#.......
#.....#..#
.#..#..#.#
#....##...
....#.#...
.#.......#
..###.##..

Tile 3881:
...#.#..#.
.#..##.##.
#.##.#.#..
.#..#.....
.....#.#.#
###....#.#
#..#.....#
......#..#
....#.....
####..#.##

Tile 1931:
#.....#..#
.#.......#
#.##.#..#.
..........
#.#.#.....
##......##
.#........
###..#.#..
#.#.#....#
.#.#.###.#

Tile 1907:
#...###.##
#.....#.#.
......#..#
#........#
#.........
#.#.##...#
##..#.....
..#......#
.#.##.#...
#.##.#####

Tile 3329:
.##.#..#.#
#....##..#
.....#..#.
..##......
#...#.##..
......#...
...#.#.#..
#...#.....
##.##....#
..##......

Tile 1009:
#...#.#.#.
..#...##..
#....##.##
##...##...
#.#..##...
##.##...##
##...##.#.
..#..#...#
..###.####
....##..#.

Tile 2011:
...#...#..
#..#.....#
.#......##
##...###..
#...#.....
..........
#.#.#....#
###....#..
.#...##...
####..###.

Tile 3467:
##..#.####
##.#.....#
#.#..#.#..
.##....###
#........#
..........
.###..#..#
......#..#
..#.....##
.###.#....

Tile 3533:
.#.###...#
#...#..##.
.##.##...#
..#.####.#
..#..#...#
.#..#.#...
#........#
.#..#....#
....#...#.
##.###...#

Tile 3203:
#..#######
...#....#.
...###...#
....#.....
.......#.#
........##
#.##.#..#.
..###...##
#...##....
#..###..#.

Tile 1307:
#..##..###
.#.##....#
....##...#
..#......#
#.##......
###.#..#..
##.##...#.
##.#..####
#...#....#
######.#..

Tile 1087:
#...#####.
..#...#.#.
#.##......
.#....#..#
#.........
..##..#..#
##.......#
#.##.#..##
#........#
#...##.###

Tile 1879:
#.#...#.##
..#...##.#
#....##..#
#........#
#.........
#...#.....
#..#.###.#
##..#.....
#...#.....
..##.#.#..

Tile 1733:
###.##.#..
..##.#.#.#
...#......
....#.#..#
.....#...#
#...#....#
#...#####.
#..#..#...
#..#.....#
#.....##.#

Tile 2357:
.##.##.#.#
..##.##.#.
##.#.#...#
..#.....#.
...#..##..
.#......#.
.......#.#
##..######
#.........
#.#.######

Tile 3929:
...##....#
.##.###...
#.....##.#
..#.#..#..
..#..#....
#....#..##
#...#.#..#
.....###..
#.........
######.#.#

Tile 2621:
..#######.
....#..#..
#......###
#......#.#
#.......##
........#.
..##...#.#
.####.##.#
..#####..#
###.....##

Tile 1787:
....#.##..
......###.
..#.##.#.#
###....##.
#.#..#...#
...###.#.#
#......###
#.#....#..
...#...##.
###.#.##.#

Tile 1549:
#.#####.##
..##...###
#..#.#...#
.#...##..#
.#....##..
.......#.#
........##
..#.#.....
..........
....######

Tile 1933:
.####.####
#...#....#
#......##.
....#...##
###..#...#
##.#.##...
#....#..##
...#......
##........
#.#..#.##.

Tile 3557:
#..#.####.
#....#.#.#
#.....##..
.#.#....#.
.##.#..##.
#.##.#...#
..#....##.
#..#......
##..#...##
.#.##.#..#

Tile 3631:
###...##.#
.##....#..
#..##....#
......#...
...#..#..#
...#.....#
.........#
##..#..#.#
##.###....
##.#...#..

Tile 3877:
.##..#.##.
..#.#..#..
......#...
...#...#.#
.........#
#..#.....#
##.#...#.#
#......#.#
#.#.......
#####..##.

Tile 2909:
#..##...#.
###.#....#
#.##......
...#.#.###
#.#.##.###
....#..#..
...#.....#
##........
#.#......#
##.#..#..#

Tile 1873:
#..#.##.##
.#...#.#.#
..........
#.##.#..#.
.#..#.##.#
#..#....##
.......#..
.......#.#
##.#....#.
.#########

Tile 3539:
###.#..#..
##.....###
#...#.#..#
#....#.#.#
#..#......
......#...
....#....#
#...#...#.
....##.###
.#..#..#.#

Tile 1657:
..##..###.
#.......#.
.##.....#.
#..#.##..#
#.....#...
..#.......
#.#...#.##
......#...
.#.#######
##.....#..

Tile 2753:
#..#.#####
.#.###.#.#
..##....#.
###.....#.
.#.#.##...
##...#.#.#
##.###..#.
#####..#..
#.##...#..
..######.#

Tile 1409:
####.##..#
..#..#.#..
........##
..#...#.#.
.##....#.#
#..#.....#
.........#
#..##.....
#.##.#..##
.#.#.#.###

Tile 2243:
..#.##..##
.........#
#....##.#.
.#.#....##
##...#....
.#....#.##
#....#.#..
#.#..#....
#..#...#..
....#####.

Tile 3541:
#.#...##.#
#..#..#.##
..##.##..#
.....#.#.#
....#.####
#...#..##.
...#...##.
#....#..##
..##....#.
..#..#.#.#

Tile 3229:
.....#..#.
#...#..#..
..#.#.....
##.#..#.##
##.....#.#
..#.#.##..
...#...###
.##.......
..##......
..#.##.#.#

Tile 1019:
..####.#.#
....###..#
##........
###.....#.
##..#..#.#
.....##...
..#...#..#
......#...
#..#.....#
.....####.

Tile 1663:
#.##.#####
#.##..##.#
#.......##
#...#.##.#
......#...
#...#....#
.....#..##
#.#.#.##.#
....#..#.#
#.###...##

Tile 1567:
.##.##....
..#..#..##
.#....##.#
....#...#.
#......#..
#..#....#.
#...##....
....#...#.
..#....###
#.#.#.##.#

Tile 3833:
.#.....#.#
#.........
#...#.....
.#..#....#
#..#.#....
#....###..
...###...#
.#.#.#....
##..#....#
#..#.#..#.

Tile 2551:
###.#....#
.....##...
.#.......#
..###..###
#.#...#..#
#......#.#
.#..###...
.......#..
#.#..#...#
###...##.#

Tile 1637:
###.##...#
..#..#....
..###.##..
#........#
#.#......#
......####
#....#.###
#.###.....
.#.#.....#
##.##.####

Tile 2281:
#####..#..
.##.#..###
#..#.##...
.#.#.#....
#.#.#...##
..##.##...
.....##..#
..#..##...
##..#..#.#
##....#..#

Tile 3559:
##.#.#.#.#
.....#.#..
#....#...#
....#.....
###....#.#
..........
##..#....#
.....###.#
#.........
...#......

Tile 3511:
.##.####..
#.........
##.....#..
#......##.
##.....#.#
#.....#.##
.#..#.....
#...#.#..#
#..#.#.#..
.###.#.###

Tile 2063:
.....#.###
##.#...#.#
#....##..#
...#.#...#
#.#.#....#
.#........
..#..###.#
#...###...
.#....##..
..#.###.##

Tile 3187:
......##..
.#....#...
...#.####.
.##.#...#.
#........#
.#...#..##
.#....#...
#..#####.#
....##..#.
#..##.#..#

Tile 3407:
#.###...##
.#........
..##..#..#
.##.#...##
.....#.###
#.#....#.#
.#..#.#...
.#.#..#..#
#..#.#.#.#
...#.#.###

Tile 1597:
.#..#..#..
..#.......
.#.###.#..
##..#...#.
.........#
#.##..##..
....#.....
.#..#...##
#.....#.##
###....#.#

Tile 2087:
#.#..##.##
.##......#
.......#.#
.....#..##
#..#.....#
.#.......#
......#.##
.##....#.#
.......#..
#.#.###.##

Tile 2749:
#####..#.#
#..#...###
...#.#...#
#......#..
##.......#
.#.......#
.#.......#
#..#..#...
.###..#.##
.##.....#.

Tile 1697:
.##.###...
.##..#...#
##....#..#
#....#..#.
.........#
...#..##.#
#....#....
#....##...
..#.##.#..
##....##..

Tile 2711:
##.##.##..
.#..#....#
.#...#....
...#.#....
.##.....#.
##.#..#...
..#...####
##...##.##
.......##.
..#....###

Tile 2521:
.##......#
....##....
.........#
#.##..#...
..........
..#.......
.#........
...#..#...
.......#..
##....#.##

Tile 3163:
.##.#.....
#.........
#........#
#...##.#.#
.#........
.#.#......
........#.
#......###
###.#..#..
###.####.#

Tile 3343:
.#..######
#######..#
...#...#.#
#..#.#...#
.#..##..#.
#...#..#.#
..........
#.###...#.
#..#...##.
#.#.##..#.

Tile 1303:
...#.####.
#..#......
###.#....#
.#.#.....#
.#..#....#
..#.....##
.......#.#
..#..#..##
##..#.##.#
#..#....#.

Tile 3491:
#..##..#..
#.#....##.
#...#..#..
#.#.#.#...
.###.#...#
#....#....
...#..#..#
.#..##.#.#
#.#....###
##...#...#

Tile 3767:
.##..#.#..
#..#....#.
......#...
...#.#..#.
##......##
..#....#..
......##..
#..#....#.
..........
..#..#.#.#

Tile 1823:
...#.##.#.
....#.....
.###......
...#.#....
#..#..#...
#..#.#..##
#...#.....
.#..#....#
..###.#...
.#..#.#.#.

Tile 3863:
.#####.#.#
#.##.#####
...#.#....
..#.#.#..#
#........#
#..#.##...
...##....#
###..##.##
#....#.#..
...#.#####

Tile 1867:
..#.#...#.
##.####...
..........
##.#..#...
..#....#.#
#....#.##.
#..##..#.#
....##..##
...#......
#....##.#.

Tile 2341:
#.#....#..
#........#
..#.....##
.....#.###
....#.#.#.
#..#..##.#
#..##....#
..#..#..#.
#.......##
#.##.####.

Tile 2411:
.#..##....
##.##..#.#
##..#.#...
##...#..##
....#....#
#..#..#...
.....##..#
....#..#..
..#.#..#.#
..##...###

Tile 1523:
...#.##.#.
.#.##....#
#..##.#..#
.#...#....
#......###
#..#......
#.#.#.....
##..##....
..........
.#.##..##.

Tile 2609:
###...####
##...##..#
.#.#.#.##.
.#...##...
##..####.#
...##.##.#
..#...##..
......#..#
#.#..#...#
..##...##.

Tile 2287:
#######.##
#....##.#.
.#..#..#.#
#..##....#
#....####.
....##....
##..##..#.
#..##.....
..##.....#
#..##.##.#

Tile 1049:
#.##......
...#..#..#
.#....###.
###.####..
.......#.#
..#..#..#.
.......#..
.##...#...
....#..##.
.#####...#

Tile 1187:
###....##.
.##.#.#..#
..#.......
##..##.#..
####..#.##
..........
..#.......
.........#
##.#......
...#####.#

Tile 2707:
##.###..##
....###..#
#.##.#....
...#.##.#.
.##..#....
...#..##..
......#..#
...#.#.#..
.......#..
.###.#.#..

Tile 2441:
#.#...#.#.
.####...#.
#.#.......
.....#....
#..#...#.#
...#....##
##.....#..
#..##.....
#..#....#.
.#.##....#

Tile 3571:
.#.###...#
...#.#...#
###...##..
#..###.#..
#..#...#..
....#.##..
#...#.#..#
#..##.....
#.#.....#.
##..#.#.##

Tile 2969:
.######...
#.#...#..#
....#....#
...#..##..
#..####...
...#.#...#
#..##..#.#
..#.......
#..#.##...
.#.##.##.#

Tile 2699:
#..###..#.
.#.#......
..#..#.#..
.....###..
.........#
..##.##..#
.....#...#
#.#..##...
#...#...##
###..#..##

Tile 1759:
#......###
.........#
#...###..#
###......#
####....#.
..........
.......#..
.#.....#.#
#.#.....#.
...##.###.

Tile 2843:
..#.###.##
.....#...#
..#......#
#..#..#...
...###.#..
..##..#..#
...#..#...
#....#..##
#.#.#.#..#
####......

Tile 1063:
...#.#.#.#
.#.##.....
##...#..##
#.#.###..#
.#..#..#.#
...####..#
#.###..#.#
..#..#....
####....##
...##.....

Tile 2161:
.#.#..#..#
...#..#.##
.#..##.#..
#........#
..#.......
#......#..
.##.....##
.#.#.##...
.#.###...#
#.#.#.#.#.

Tile 1543:
#..##.##..
#...#.....
#..###..##
#....#..##
####......
###.......
.#........
##.#......
##....#...
..#.####.#

Tile 1979:
..#.#....#
#..##.....
.#.##....#
##........
....#.#..#
#...#.....
#......#.#
#...#....#
.#..#....#
#..###.#..

Tile 1607:
#..##.....
#.......##
#..#.##.#.
.......###
.#..##....
....##..##
##..#.....
#....#.###
.........#
.#...####.

Tile 2957:
#####...##
#.#...#.##
##....###.
..........
#.......#.
...#...##.
.......###
##..##.#.#
.#..###.#.
.#.###..##

Tile 3221:
..#.#.....
.#...#.#.#
....##..##
#..##...##
..........
.#........
..#.......
#..#.....#
...##.#.##
...####..#

Tile 1213:
#...#.####
.........#
#.##..#..#
..#...#...
##.##....#
.....#....
.........#
#...##..##
#.......##
####..#.#.

Tile 3023:
##..#..###
..###.#.#.
##.#......
.....#...#
..#..#..#.
##........
##.#.#.#..
...####.##
.....#....
#.##....#.

Tile 1489:
.###..#...
#........#
##...#.#..
##..##..#.
....#....#
.....##.#.
#...######
#....#...#
...#.###..
#.#.#.#.##

Tile 1601:
..#...##.#
........#.
#...#....#
#....#....
###.#.#..#
...#####.#
...####.#.
#....##...
.#...#...#
...#.#.#.#

Tile 2953:
#.#...###.
#..#...#..
.........#
#.#.#....#
#..##.#..#
#..#......
##.#..#.##
#.#..#..#.
.........#
...##...#.

Tile 1481:
......#...
......#.#.
#........#
..........
#..##....#
.#..#...##
.##.....#.
##..#.#...
#......#..
##..###.##

Tile 1531:
..#.#...#.
.....##...
.#....#.#.
..#....#..
#.#.###...
#....###..
........#.
#....#.#..
##.#......
#...###..#

Tile 1039:
##.#..#...
#.#...#...
#.#.#...##
.#.#.##..#
##...##..#
##..##..#.
#.........
#.#.#..#..
.#..#..#..
.##......#

Tile 2549:
.##.####.#
#..#...#..
#..#..#.#.
...##.#..#
#..##.#..#
##.#.#.###
...##..#.#
......####
..##.#..#.
######.#..

Tile 1913:
..#....#.#
#....##..#
#.#....#..
...##.....
.....#.###
....#..#..
##...#.#..
..#...#...
#...#...#.
.##..###.#

Tile 1831:
..#.....##
##.#..##.#
....###.##
#.........
####.#..#.
###..#..#.
#..##....#
#.......##
....##...#
##.##.#..#

Tile 2099:
...##.##.#
..#.....##
.....#...#
..#..#..##
.#####..#.
.....#...#
..#.....#.
.....#...#
.#...#...#
#.#..#....

Tile 3209:
####..#...
....#....#
....#.#..#
....##...#
....#.....
.#.....#..
.###...#..
...#....##
#.#......#
..#.#..##.

Tile 2593:
#.####.#..
###...####
#.......#.
##.##...#.
#.....#.#.
...#...#.#
#.#.#....#
###....###
#...#....#
.###..####

Tile 3011:
#.#.####.#
##.#....##
##.......#
.##.#.....
.#....###.
.##...##..
.#...#.#.#
##...#..#.
#.#....#..
.#..#.#.##

Tile 2371:
##.#####..
#........#
#.#...#...
##...#.###
#.......##
...#....#.
#.#......#
..####.###
#...#..#.#
##.##...#.

Tile 3853:
###.#.#.##
.........#
#..#.#.#..
#........#
#...##...#
##...###.#
.#.###....
...##....#
##..#.##.#
##..#.#.#.

Tile 3821:
..#.##...#
#.##....#.
.#.#####.#
#.#..#.#.#
..###....#
...###...#
##.......#
.##..#..#.
###......#
#...#..###

Tile 2591:
##...#..##
#.##.#..#.
#..###...#
....#...##
#.......#.
....##...#
...#.##...
.##....###
......#..#
#.#..#.#.#

Tile 3041:
#.###..#.#
...#......
...##.#..#
.....##.#.
.#..#..##.
....#....#
#.#.......
...#####.#
#..##..##.
#.###...#.

Tile 1667:
####...#..
..........
..........
#.#..##...
..#..#....
..#...#..#
......##.#
#.#....##.
.....#...#
..########

Tile 2003:
..##..#.##
###.#..#.#
...#....##
.........#
#........#
.#......##
##..##.#.#
##..#..#.#
..#....###
#...#.#...

Tile 1447:
#####....#
..#......#
#......#.#
#..#.....#
#..#.####.
#....#.#..
#..####...
#....##...
#.#.......
.###..##..

Tile 2333:
##.##.###.
#....#..##
#....#....
#.#.......
..........
....#....#
#..#...###
...#.....#
#..##.#..#
###...#.#.

Tile 1571:
.....##.#.
......##.#
..#......#
..........
#.....#...
#..#..#...
#.#...#...
....##....
#..#..##..
#....##...

Tile 1181:
..#...##..
##.......#
#..##.#...
###...#.#.
#.#..#.#..
###....##.
.....#....
##..#.....
.....#..#.
#...#.##.#

Tile 1117:
###.#...#.
#.........
...#....#.
#..##..###
##....#.##
.....##..#
....##..##
.#.###...#
#.#.......
.##...#.#.

Tile 3623:
#..#######
#....###..
.#....#...
#....#.##.
..#.......
#..######.
#.........
#...#.##.#
....#...#.
#####.###.

Tile 2111:
##.#..#..#
#..#..##..
#..####...
#......#.#
#...##...#
......#.#.
..........
....#.#.#.
..........
.###......

Tile 2579:
.....#..#.
#.#.....#.
#..#.....#
#........#
....#..#..
......##.#
..##.#.#.#
...#..####
.#........
#.##......

Tile 2477:
.#..######
.#........
..#......#
#...##...#
...#.....#
#..##.#..#
..#.#....#
##.#.....#
.#.#......
.##...##..

Tile 1621:
.#.##.####
#........#
....#.#.##
#...#.#..#
.........#
.#....#..#
....#....#
#......#.#
..##....#.
##.##.##..

Tile 2297:
.#..##.#..
...#....##
....###..#
#..#...##.
#.##......
.###......
#.......##
......#...
.#....#.#.
#......#..

Tile 2237:
##.#.#.###
#..#....#.
.#..##...#
..#..##...
.......#..
......##.#
...#..#...
.....###.#
#....#....
.##.#.####

Tile 1097:
##.#...#..
..#...#...
#..##.....
.#....#..#
...##....#
..#.###.#.
......#..#
#..#..##..
#..##.#...
.#.#..##..

Tile 3037:
###.##.#..
##..#..#.#
#........#
..#.....##
....#.....
...#.#..#.
#.##.....#
..........
.##....#.#
.#.#.#...#

Tile 2617:
##....###.
##.#...#.#
........#.
.......#..
#....#....
...#....##
......#.##
..####...#
.#...#..##
.##..##..#

Tile 2273:
...#.###.#
#.##.#..##
.....#...#
.#.#....##
#.....#..#
#.#......#
...#.#....
...##..###
##.....#.#
##....###.

Tile 1321:
.##..#####
#........#
#....#...#
...#...#..
.....#....
#.#...#..#
#......#..
##..#.#..#
.##....#.#
#.####.#.#

Tile 3299:
##....#.##
..........
#.#.#.#..#
..#......#
...#......
....#....#
#.......##
...#.....#
...#...#.#
.#...##...

Tile 2467:
#.#......#
#.....#..#
##.###.#..
..#..##..#
.#..#....#
.#..#...#.
..#....#..
.#....#...
#.#..#...#
#..#####.#

Tile 3313:
#.##.....#
#....#.#..
.........#
#.#.##.#..
##...#.#..
#..#..##..
#.....##.#
##...##...
#...##.###
#...#.##.#

Tile 2677:
..##.#.##.
.........#
..#...#.##
..#..###..
##.....###
..#......#
..#.#.#..#
.###....##
#..##.##..
...#.##.##

Tile 2207:
.####.####
##..#.....
#.#..#.#..
#.##.#..##
#...#.#..#
.#.....#.#
......#..#
....##..##
.....#....
...##..##.

Tile 3769:
###.##..#.
##...#....
#.........
#...#..##.
.#...#.#..
..###....#
.........#
#...##...#
###.#.#.#.
.##..##...")

(defun row-major-index (width x y)
  (+ x (* y width)))

(defstruct point
  (x 0 :type integer)
  (y 0 :type integer))

(defun point (x y)
  (make-point :x x :y y))

(defun point-le (p1 p2)
  (and (<= (point-x p1)
           (point-x p2))
       (<= (point-y p1)
           (point-y p2))))

(defstruct dims
  (width 0 :type integer)
  (height 0 :type integer))

(defun dims (width height)
  (make-dims :width width :height height))

(defstruct bounds
  (point nil :type point)
  (dims nil :type dims))

(defun bounds (point dims)
  (make-bounds :point point :dims dims))

(defun bounds-x (bounds)
  (point-x (bounds-point bounds)))

(defun bounds-y (bounds)
  (point-y (bounds-point bounds)))

(defun bounds-point2 (bounds)
  (with-slots (point dims) bounds
    (with-slots (x y) point
      (with-slots (width height) dims
        (point (+ x width)
               (+ y height))))))

(defun bounds-width (bounds)
  (dims-width (bounds-dims bounds)))

(defun bounds-height (bounds)
  (dims-height (bounds-dims bounds)))

(defgeneric containsp (outer inner))

(defmethod containsp ((outer bounds) (inner bounds))
  (and (point-le (bounds-point outer)
                 (bounds-point inner))
       (point-le (bounds-point2 inner)
                 (bounds-point2 outer))))

(defmethod containsp ((outer bounds) (inner point))
  (and (point-le (bounds-point outer) inner)
       (point-le inner (bounds-point2 outer))))

(defmethod containsp ((outer dims) (inner bounds))
  (containsp (bounds (point 0 0) outer)
             inner))

(defclass image ()
  ((bits
    :documentation "The image data as a bit-vector."
    :initarg :bits
    :type bit-vector
    :reader image-bits
    :initform (error "Must supply :bits"))
   (dims
    :documentation "The dimensions (width and height) of the image."
    :initarg :dims
    :type dims
    :reader image-dims
    :initform (error "Must supply :dims"))))

(defmethod initialize-instance :after ((object image) &key)
  (with-slots (bits dims) object
    (assert (= (length bits) (* (dims-width dims)
                                (dims-height dims))))))

(defmethod print-object ((object image) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (bits dims) object
      (format stream "~S" dims)
      (when (<= (length bits) 64)
        (format stream " ~S" bits)))))

(defun image-height (image)
  (dims-height (image-dims image)))

(defun image-width (image)
  (dims-width (image-dims image)))

(defun make-zero-image (dims)
  (make-instance 'image
                 :bits (make-array (* (dims-height dims)
                                      (dims-width dims))
                                   :element-type 'bit
                                   :initial-element 0)
                 :dims dims))

(defun image-equal (a b)
  (check-type a image)
  (check-type b image)
  (with-slots ((a-bits bits) (a-dims dims)) a
    (with-slots ((b-bits bits) (b-dims dims)) b
      (and (equalp a-dims b-dims)
           (equalp a-bits b-bits)))))

(defun image-rotate (src)
  (check-type src image)
  (let* ((m (image-height src))
         (n (image-width src))
         (src-bits (image-bits src))
         (dst-dims (dims m n))
         (dst-bits (make-bits dst-dims)))
    (flet ((src-index (c r)
             (let ((i (+ r (* c n))))
               ;; (format t "~&src (~d ~d) -> ~d~%"
               ;;         c r i)
               i))
           (dst-index (c r)
             (let ((i (+ r (* c m))))
               ;; (format t "~&dst (~d ~d) -> ~d~%"
               ;;         c r i)
               i)))
      (loop for r below m do
        (loop for c below n do
          ;; (format t "~&at (~d ~d) src ~d -> dst ~d~%"
          ;;         r
          ;;         c
          ;;         (src-index r c)
          ;;         (dst-index c (- m 1 r)))
          (setf (bit dst-bits (dst-index c (- m 1 r)))
                (bit src-bits (src-index r c))))))
    (make-instance 'image :dims dst-dims :bits dst-bits)))

(defun image-bit-op (function a b)
  (check-type a image)
  (check-type b image)
  (with-slots (bits dims) a
    (assert (equalp dims (image-dims b)))
    (make-instance 'image
                   :bits (funcall function bits (image-bits b))
                   :dims dims)))

(defun image-bit-and (a b)
  (image-bit-op #'bit-and a b))

(defun image-bit-ior (a b)
  (image-bit-op #'bit-ior a b))

(defun flip-image (src)
  (check-type src image)
  (let* ((dims (image-dims src))
         (m (dims-height dims))
         (n (dims-width dims))
         (src-bits (image-bits src))
         (dst-bits (make-bits (image-dims src)))
         (max-col (1- n)))
    (flet ((index (c r) (+ r (* c n))))
      (loop for row below m do
        (loop for col below n do
          (setf (bit dst-bits (index row col))
                (bit src-bits (index row (- max-col col)))))))
    (make-instance 'image :dims dims :bits dst-bits)))

(defun format-image (image)
  (check-type image image)
  (with-slots (dims bits) image
    (loop with width = (dims-width dims)
          for i below (length bits)
          for x from 0
          do (progn
               (setf x (mod x width))
               (when (zerop x)
                 (format t "~&"))
               (format t "~c" (ecase (bit bits i)
                                (1 #\#)
                                (0 #\.))))
             finally (format t "~&"))))

(defun image-to-string (image)
  (with-output-to-string (*standard-output*)
    (format-image image)))

(defun extract-image (image bounds)
  (check-type image image)
  (with-slots ((src-dims dims) (src-bits bits)) image
    (assert (containsp src-dims bounds))
    (let* ((src-x (bounds-x bounds))
           (src-y (bounds-y bounds))
           (src-width (dims-width src-dims))
           (dst-dims (bounds-dims bounds))
           (dst-width (bounds-width bounds))
           (dst-bits (make-bits dst-dims)))
      (loop for y below (dims-height dst-dims) do
        (loop for x below dst-width do
          (setf (bit dst-bits (row-major-index dst-width x y))
                (bit src-bits (row-major-index src-width
                                               (+ x src-x)
                                               (+ y src-y))))))
      (make-instance 'image :dims dst-dims :bits dst-bits))))

;; static int[][] rotateCW(int[][] mat) {
;;     final int M = mat.length;
;;     final int N = mat[0].length;
;;     int[][] ret = new int[N][M];
;;     for (int row = 0; row < M; row++) {
;;         for (int col = 0; col < N; col++) {
;;             ret[col][M-1-row] = mat[row][col];
;;         }
;;     }
;;     return ret;
;; }

;; 0  1  2  3
;; 4  5  6  7
;; 8  9 10 11

;;  8  4  0
;;  9  5  1
;; 10  6  2
;; 11  7  3

;;;; 8 4 0  9 5 1  10 6 2  11 7 3

(deftype array2d () '(array * (* *)))
;; (deftype bit-array2d () '(array bit (* *)))

;; (defun array2d-to-image (array)
;;   (check-type array bit-array2d)
;;   (make-instance 'image
;;                  :height (array-dimension array 0)
;;                  :width (array-dimension array 1)
;;                  :bits (make-array (* (array-dimension array 0)
;;                                       (array-dimension array 1))
;;                                    :element-type 'bit
;;                                    :displaced-to array)))

;; (defun mirror-array2d (a)
;;   (check-type a array2d)
;;   "Return a mirror of a two dimensional array. The rows are reversed."
;;   (let* ((b (make-array (array-dimensions a) :element-type (array-element-type a)))
;;          (maxi (1- (array-dimension a 0))))
;;     (loop for j from 0 below (array-dimension a 1) do
;;       (loop for i from 0 to maxi do
;;         (setf (aref b i j) (aref a (- maxi i) j))))
;;     b))

;; (defun rotate-array2d (a)
;;   (check-type a array2d)
;;   (let ((b (make-array (reverse (array-dimensions a))
;;                        :element-type (array-element-type a)))
;;         (m (array-dimension a 0))
;;         (n (array-dimension a 1)))
;;     (loop for i below m do
;;       (loop for j below n do
;;         (setf (aref b (- n 1 j) i)
;;               (aref a i j))))
;;     b))

;; (defun extract-array2d (a start-y start-x dim-y dim-x)
;;   (check-type a array2d)
;;   (assert (<= 0 start-y (array-dimension a 0)))
;;   (assert (<= 0 start-x (array-dimension a 1)))
;;   (assert (<= 0 (+ start-y dim-y) (array-dimension a 0)))
;;   (assert (<= 0 (+ start-x dim-x) (array-dimension a 1)))
;;   (let ((e (make-array (list dim-y dim-x)
;;                        :element-type (array-element-type a))))
;;     (loop for y below dim-y do
;;         (loop for x below dim-x do
;;           (setf (aref e y x)
;;                 (aref a (+ start-y y) (+ start-x x)))))
;;     e))

(defun above (&rest images)
  (let* ((width)
         (vectors (loop for image in images
                        do (check-type image image)
                        if width
                          do (assert (= width (image-width image)))
                        else
                          do (setf width (image-width image))
                        collect (image-bits image)))
         (bits (make-array (reduce #'+ vectors :key #'length)
                           :element-type 'bit)))
    (loop with i = 0
          for vector in vectors
          do (loop for bit across vector
                   do (setf (bit bits i) bit)
                   do (incf i))
          finally (assert (= i (length bits))))
    (make-instance 'image :dims (dims width
                                      (/ (length bits) width))
                          :bits bits)))

(defun beside (&rest images)
  (let* ((height)
         (bits (make-array (reduce #'+ images
                                   :key #'(lambda (image)
                                            (length (image-bits image))))
                           :element-type 'bit)))
    (loop for image in images
          do (check-type image image)
          if height
            do (assert (= height (image-height image)))
          else
            do (setf height (image-height image)))
    (loop with i = 0
          for row below height
          do (loop for image in images
                   do (loop with src-width = (image-width image)
                            with start-i = (* row src-width)
                            with end-i = (+ start-i src-width)
                            with src-bits = (image-bits image)
                            for src-i from start-i below end-i
                            do (setf (bit bits i) (bit src-bits src-i))
                            do (incf i)))
          finally (assert (= i (length bits))))
    (make-instance 'image :dims (dims (/ (length bits) height)
                                      height)
                          :bits bits)))

;; (defun stack-vertical-array2d (a b)
;;   (check-type a array2d)
;;   (assert (= (array-dimension a 1)
;;              (array-dimension b 1)))
;;   (let* ((a-xdim (array-dimension a 1))
;;          (a-ydim (array-dimension a 0))
;;          (b-ydim (array-dimension b 0))
;;          (stacked (make-array (list (+ a-ydim b-ydim) a-xdim)
;;                               :element-type (array-element-type a))))
;;     (loop for x below a-xdim
;;           do (loop for y below a-ydim
;;                    do (setf (aref stacked y x)
;;                             (aref a y x)))
;;           do (loop for y below b-ydim
;;                    do (setf (aref stacked (+ y a-ydim) x)
;;                             (aref b y x))))
;;     stacked))

;; (defun m2d (d0 d1)
;;   (let ((a (make-array `(,d0 ,d1))))
;;     (dotimes (i (* d0 d1))
;;       (setf (row-major-aref a i) i))
;;     a))

(defconstant +top+ 0)
(defconstant +right+ 1)
(defconstant +bottom+ 2)
(defconstant +left+ 3)
(defconstant +sides+ 4)
(defconstant +side-bit-length+ 10
  "The bit count for a tile side.")

(deftype side-array () `(simple-vector ,+sides+))

(defclass tile ()
  ((id
    :documentation "The ID of this tile."
    :initarg :id
    :type integer
    :reader tile-id
    :initform (error "Must supply :id."))
   (image
    :initarg :image
    :type image
    :reader tile-image
    :initform (error "Must supply :image."))
   (sides
    :reader tile-sides
    :type side-array)))

(defmethod initialize-instance :after ((object tile) &key)
  (with-slots (image sides) object
    (let ((height (image-height image))
          (width (image-width image)))
      (setf sides
            (map 'vector
                 #'image-bits
                 (mapcar
                  #'(lambda (bounds)
                      (extract-image image bounds))
                  (list
                   ;; bounds for top, right, bottom, left.
                   (bounds (point 0 0)
                           (dims width 1))
                   (bounds (point (1- width) 0)
                           (dims 1 height))
                   (bounds (point 0 (1- height))
                           (dims width 1))
                   (bounds (point 0 0)
                           (dims 1 height)))))))))

(defmethod tile-side ((object tile) index)
  (aref (slot-value object 'sides) index))

(defmethod print-object ((object tile) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (id) object
      :do (format stream "id: ~d" id))))

(defun format-tile (tile)
  (with-slots (id image) tile
    (unless (zerop id)
      (format t "~&Tile: ~d~%" id))
    (format-image image)))

(defun format-spaced-image-row (images)
  (loop for y below (image-height (elt images 0)) do
    (loop initially (format t "~&")
          with need-space-p = nil
          for image in images do
            (progn
              (if need-space-p
                  (format t " ")
                  (setq need-space-p t))
              (loop with width = (image-width image)
                    with bits = (image-bits image)
                    for x below width do
                      (format t "~c" (ecase (bit bits
                                                 (row-major-index width x y))
                                       (1 #\#)
                                       (0 #\.))))))))

(defun format-image-grid (image-grid)
  (check-type image-grid array2d)
  (format t "~&")
  (loop for m below (array-dimension image-grid 0)
        unless (zerop m)
          do (format t "~%")
        do (format-spaced-image-row
            (loop for n below (array-dimension image-grid 1)
                  collect (aref image-grid m n)))
        do (format t "~%")))

(defun tile-equal (tile-a tile-b)
  (and (= (tile-id tile-a)
          (tile-id tile-b))
       (equalp (slot-value tile-a 'sides)
               (slot-value tile-b 'sides))))

(defun split-sequence (needle haystack &key (start 0) end)
  (loop with needle-length = (length needle)
        for haystack-start = start then (+ pos needle-length)
        for haystack-end = end
        for pos = (search needle haystack
                          :start2 haystack-start
                          :end2 haystack-end)
        collect (subseq haystack haystack-start pos)
        until (null pos)))

(defun split-lines (str)
  (split-sequence '(#\Newline) str))

(defun split-tiles (str)
  (split-sequence '(#\Newline #\Newline) str))

(defun parse-tile-id (str)
  (prog1 (parse-integer str
                        :start (position-if #'digit-char-p str)
                        :junk-allowed t)))

(defun make-bits (dims)
  (make-array (* (dims-width dims) (dims-height dims)) :element-type 'bit))

(defun parse-image (rows)
  (let* ((dims (dims (length (elt rows 0))
                     (length rows)))
         (bits (make-bits dims))
         (index 0))
    (map nil #'(lambda (row)
                 (assert (= (dims-width dims) (length row)))
                 (map nil #'(lambda (e)
                              (setf (bit bits index) (ecase e
                                                       (1 1)
                                                       (0 0)
                                                       (#\O 1)
                                                       (#\# 1)
                                                       (#\. 0)))
                              (incf index))
                      row))
         rows)
    (make-instance 'image :dims dims :bits bits)))

(defun parse-tile (str)
  (destructuring-bind (id &rest rows) (split-lines str)
    (make-instance 'tile :id (parse-tile-id id)
                          :image (parse-image rows))))

(defun parse-tiles (str)
  (mapcar #'parse-tile (split-tiles str)))

(defun permute-image (image)
  (delete-duplicates
   (loop for img in (list (flip-image image) image)
         collect img
         collect (setq img (image-rotate img))
         collect (setq img (image-rotate img))
         collect (setq img (image-rotate img)))
   :test #'equalp))

(defun permute-tile (tile)
  (mapcar #'(lambda (image)
              (make-instance 'tile
                             :id (tile-id tile)
                             :image image))
          (permute-image (tile-image tile))))

(defun permute-tiles (tiles)
  (loop for tile in tiles
        nconc (permute-tile tile)))

(defun make-side-table (tiles)
  "Returns a data structure that makes it easy to find tiles by side index
and color.  See FIND-TILES-BY-SIDE."
  (flet ((make-index (side-index)
           (let ((table (make-hash-table :test 'equalp)))
             (loop for tile in tiles
                   do (push tile
                            (gethash (tile-side tile side-index) table)))
             table)))
    (make-array +sides+ :initial-contents (loop for side below +sides+
                                                collect (make-index side)))))

(defun find-tiles-by-side (side color table)
  "Given a SIDE index [0..3] and COLOR, returns a list of tiles that match.
The TABLE is that created by MAKE-SIDE-TABLE."
  (gethash color (aref table side) nil))

(defun place-tiles (tiles)
  (let* ((tile-count (length tiles))
         (stride (ceiling (sqrt tile-count)))
         (placed-array (make-array tile-count :fill-pointer 0))
         (placed-hash-table (make-hash-table))
         (side-table (make-side-table (permute-tiles tiles))))
    ;; (format t "tile-count ~d stride ~d~%" tile-count stride)
    (assert (= tile-count (* stride stride)))
    (labels
        ((push-tile (tile)
           ;; (format t "pushing at ~d : ~a~%" (length placed-array) tile)
           (vector-push tile placed-array)
           (assert (not (gethash (tile-id tile) placed-hash-table nil)))
           (setf (gethash (tile-id tile)
                          placed-hash-table)
                 t))

         (pop-tile ()
           (remhash (tile-id (vector-pop placed-array))
                    placed-hash-table))

         (placedp (tile)
           (gethash (tile-id tile) placed-hash-table nil))

         (required-left-color (index)
           "Returns the color the left side of the tile at INDEX must be.
I.e. the color of the right side of the tile to the left of it.  Returns
nil if INDEX is in column zero (i.e. there is no tile to the left)."
           (if (= 0 (mod index stride))
               nil
               (tile-side (aref placed-array (1- index)) +right+)))

         (required-top-color (index)
           "Returns the color the top side of the tile at INDEX must be.
I.e. the color of the bottom side of the tile above it.  Returns nil if
INDEX is in the top row."
           (if (< index stride)
               nil
               (tile-side (aref placed-array (- index stride))
                          +bottom+)))

         (candidate-tiles (index)
           (if (= 0 index)
               (permute-tiles tiles)
               (let* ((left-color (required-left-color index))
                      (top-color (required-top-color index))
                      (left-tiles (if left-color
                                      (find-tiles-by-side
                                       +left+ left-color side-table)))
                      (top-tiles (if top-color
                                     (find-tiles-by-side
                                      +top+ top-color side-table))))
                 (assert (or left-color top-color))
                 ;; (format t "candidates for ~D:~%  left ~S ~S~%  top ~S ~S~%"
                 ;;         index left-color left-tiles top-color top-tiles)
                 (remove-if #'placedp
                            (cond ((and left-color top-color)
                                   (intersection left-tiles top-tiles
                                                 :test #'tile-equal))
                                  (left-color left-tiles)
                                  (top-color top-tiles))))))

         (place-tiles (index)
           (if (= index tile-count)
               ;; Done!
               (make-array (list stride stride)
                           :element-type (array-element-type placed-array)
                           :displaced-to placed-array)
               ;; Not done. Push the next tile that fits or bail if there
               ;; is none such.
               (let ((candidates (candidate-tiles index))
                     result)
                 (dolist (tile candidates)
                   (push-tile tile)
                   (if (setf result (place-tiles (1+ index)))
                       (return result))
                   (pop-tile))))))

      (place-tiles 0))))

(defun part-one (str)
  (let* ((placed (place-tiles (parse-tiles str)))
         (max-dim0 (1- (array-dimension placed 0)))
         (max-dim1 (1- (array-dimension placed 1)))
         (corners (list (aref placed 0 0)
                        (aref placed 0 max-dim1)
                        (aref placed max-dim0 0)
                        (aref placed max-dim0 max-dim1))))
    (reduce #'*
            corners
            :key #'(lambda (tile) (tile-id tile)))))

(defun sea-monster ()
  (parse-image *sea-monster*))

(defun grid-to-image (images)
  (check-type images array2d)
  (reduce #'above
          (loop for m below (array-dimension images 0)
                collect (reduce #'beside
                                (loop for n below (array-dimension images 1)
                                      collect (aref images m n))))))

(defun copy-into (from to point)
  (etypecase from
    (array2d (break)
     ;; (copy-into-array2d from to to-y to-x)
     )
    (image (copy-into-image from to point))))

(defun copy-into-image (from to point)
  (check-type from image)
  (check-type to image)
  (let ((to-x (point-x point))
        (to-y (point-y point)))
    (assert (<= 0
                to-y
                (+ to-y (image-height from))
                (image-height to)))
    (assert (<= 0
                to-x
                (+ to-x (image-width from))
                (image-width to)))
    (loop with from-bits = (image-bits from)
          with from-width = (image-width from)
          with from-height = (image-height from)
          with to-bits = (image-bits to)
          with to-width = (image-width to)
          for y integer below from-height
          do (loop for x below from-width
                     do (setf (bit to-bits
                                   (row-major-index to-width
                                                    (+ x to-x)
                                                    (+ y to-y)))
                              (bit from-bits
                                   (row-major-index from-width x y)))))))

(defun each-shifted-image (function image dims)
  (check-type image image)
  (assert (<= (image-height image) (dims-height dims)))
  (assert (<= (image-width image) (dims-width dims)))
  (loop for shift-y to (- (dims-height dims) (image-height image)) do
    (loop for shift-x to (- (dims-width dims) (image-width image)) do
      (let ((shifted (make-zero-image dims)))
        (copy-into image shifted (point shift-x shift-y))
        (funcall function shifted)))))

(defun find-subimage-mask (mask image)
  (check-type mask image)
  (check-type image image)
  (let ((found))
    (each-shifted-image #'(lambda (shifted)
                            ;; (format t "~&shifted~%")
                            ;; (format-image shifted)
                            (let ((masked (image-bit-and shifted image)))
                              ;; (format t "~&masked~%")
                              ;; (format-image masked)
                              (when (image-equal masked shifted)
                                (push shifted found))))
                        mask
                        (image-dims image))
    (if (or (null found) (= 1 (length found)))
        found
        (reduce #'image-bit-ior found))))

(defun trim-image (image)
  (with-slots (dims) image
    (extract-image image (bounds (point 1 1)
                                 (dims (- (dims-height dims) 2)
                                       (- (dims-width dims) 2))))))

(defun pad-image (image)
  (check-type image image)
  (let ((padded (make-zero-image (dims (+ 2 (image-width image))
                                       (+ 2 (image-height image))))))
    (copy-into image padded (point 1 1))
    padded))

(defun tile-grid-to-image-grid (tile-grid)
  (let ((image-grid (make-array (array-dimensions tile-grid))))
    (loop for m below (array-dimension tile-grid 0) do
      (loop for n below (array-dimension tile-grid 1) do
        (setf (aref image-grid m n)
              (tile-image (aref tile-grid m n)))))
    image-grid))

(defun trim-image-grid (image-grid)
  (check-type image-grid array2d)
  (let ((trimmed-grid (make-array (array-dimensions image-grid))))
    (loop for m below (array-dimension image-grid 0) do
      (loop for n below (array-dimension image-grid 1) do
        (setf (aref trimmed-grid m n)
              (trim-image (aref image-grid m n)))))
    trimmed-grid))

(defun trim-tile-grid (tile-grid)
  (trim-image-grid
   (tile-grid-to-image-grid tile-grid)))

(defun placement-to-image (placement)
  (grid-to-image (trim-tile-grid placement)))

(defun find-sea-monsters (image)
  (let* ((sea-monster (sea-monster))
         (images (permute-image image)))
    ;; (format t "~&Sea Monster~%")
    ;; (format-image sea-monster)
    ;; (loop for i below (length images)
    ;;       do (format t "~&~%Image ~D~%" i)
    ;;       do (format-image (elt images i))
    ;;       finally (format t "~&"))
    (loop for image in images
          for mask = (find-subimage-mask sea-monster image)
          if mask do
            (return (list image mask)))))

(defun image-bit-count (image)
  (reduce #'+ (image-bits image)))

(defun part-two (input)
  (destructuring-bind (image snakes) (find-sea-monsters
                                      (placement-to-image
    (place-tiles
     (parse-tiles input))))
    (- (image-bit-count image)
       (image-bit-count snakes))))

(defun test ()
  ;; parse-image supports row lists and row arrays.  Rows themselves
  ;; may be arrays/lists of 0 or . for zero and 1 or # for one.
  (let ((result (make-instance 'image :dims (dims 3 2) :bits #*101010)))
    (dolist (input '(((#\# #\. #\#) (#\. #\# #\.))
                     ("#.#" ".#.")
                     (#*101 #*010)
                     #(#*101 #*010)))
      (assert (image-equal result (parse-image input)))))

  ;; pad-image pads images with an edge of blank pixels.
  (assert (image-equal (parse-image '("..."
                                 ".#."
                                 "..."))
                  (pad-image
                   (parse-image '("#")))))


  ;; trim-image removes the outermost pixels.
  (assert (image-equal (trim-image
                   (parse-image '("..."
                                  ".#."
                                  "...")))
                  (parse-image '("#"))))

  (let ((shifted))
    (each-shifted-image #'(lambda (img)
                            (push img shifted))
                        (parse-image #("#."
                                       ".#"))
                        (dims 3 3))
    (setq shifted (reverse shifted))
    (assert (= 4 (length shifted)))
    (loop for actual in shifted
          for expected in (list (parse-image #("#.."
                                               ".#."
                                               "..."))
                                (parse-image #(".#."
                                               "..#"
                                               "..."))
                                (parse-image #("..."
                                               "#.."
                                               ".#."))
                                (parse-image #("..."
                                               ".#."
                                               "..#")))
          do (assert (image-equal expected actual))))

  ;; grid-to-image stacks the images as appropriate.
  (assert
   (image-equal
    (parse-image '("....#..#...."
                   "..#.#..#.#.."
                   ".#..#..#..#."
                   "....#..#...."
                   ".....##....."
                   ".#...##...#."
                   "..#..##..#.."
                   ".....##....."))
    (grid-to-image
     (make-array
      '(2 3)
      :initial-contents
      `((,(parse-image #("...."
                         "..#."
                         ".#.."
                         "...."))
          ,(parse-image #("#..#"
                          "#..#"
                          "#..#"
                          "#..#"))
          ,(parse-image #("...."
                          ".#.."
                          "..#."
                          "....")))
        (,(parse-image #("...."
                         ".#.."
                         "..#."
                         "...."))
          ,(parse-image #(".##."
                          ".##."
                          ".##."
                          ".##."))
          ,(parse-image #("...."
                          "..#."
                          ".#.."
                          "...."))))))))

  ;; Assert that the example's tile 1951, when permuted, includes the
  ;; orientation present in the example solution's top left corner.
  (assert (position
           (parse-image '("#...##.#.."
                          "..#.#..#.#"
                          ".###....#."
                          "###.##.##."
                          ".###.#####"
                          ".##.#....#"
                          "#...######"
                          ".....#..##"
                          "#.####...#"
                          "#.##...##."))
           (permute-image (parse-image '("#.##...##."
                                         "#.####...#"
                                         ".....#..##"
                                         "#...######"
                                         ".##.#....#"
                                         ".###.#####"
                                         "###.##.##."
                                         ".###....#."
                                         "..#.#..#.#"
                                         "#...##.#..")))
           :test #'image-equal))

  (assert (image-equal
           (beside
            (parse-image '("#."
                           "##"))
            (parse-image '("##"
                           ".#")))
           (parse-image '("#.##"
                          "##.#"))))

  (assert (image-equal
           (above
            (parse-image '("#."
                           "##"))
            (parse-image '("##"
                           ".#")))
           (parse-image '("#."
                          "##"
                          "##"
                          ".#"))))

  ;; The placement found for the example input is identical to that given
  ;; in the problem statement.
  (assert (string=
           (with-output-to-string (*standard-output*)
             (format-image-grid
              (tile-grid-to-image-grid
               (place-tiles
                (parse-tiles
                 *example-input*)))))
           "#...##.#.. ..###..### #.#.#####.
..#.#..#.# ###...#.#. .#..######
.###....#. ..#....#.. ..#.......
###.##.##. .#.#.#..## ######....
.###.##### ##...#.### ####.#..#.
.##.#....# ##.##.###. .#...#.##.
#...###### ####.#...# #.#####.##
.....#..## #...##..#. ..#.###...
#.####...# ##..#..... ..#.......
#.##...##. ..##.#..#. ..#.###...

#.##...##. ..##.#..#. ..#.###...
##..#.##.. ..#..###.# ##.##....#
##.####... .#.####.#. ..#.###..#
####.#.#.. ...#.##### ###.#..###
.#.####... ...##..##. .######.##
.##..##.#. ....#...## #.#.#.#...
....#..#.# #.#.#.##.# #.###.###.
..#.#..... .#.##.#..# #.###.##..
####.#.... .#..#.##.. .######...
...#.#.#.# ###.##.#.. .##...####

...#.#.#.# ###.##.#.. .##...####
..#.#.###. ..##.##.## #..#.##..#
..####.### ##.#...##. .#.#..#.##
#..#.#..#. ...#.#.#.. .####.###.
.#..####.# #..#.#.#.# ####.###..
.#####..## #####...#. .##....##.
##.##..#.. ..#...#... .####...#.
#.#.###... .##..##... .####.##.#
#...###... ..##...#.. ...#..####
..#.#....# ##.#.#.... ...##.....
"))

  (assert (string=
           (with-output-to-string (*standard-output*)
             (format-image-grid
              (trim-tile-grid
               (place-tiles
                (parse-tiles
                 *example-input*)))))
           ".#.#..#. ##...#.# #..#####
###....# .#....#. .#......
##.##.## #.#.#..# #####...
###.#### #...#.## ###.#..#
##.#.... #.##.### #...#.##
...##### ###.#... .#####.#
....#..# ...##..# .#.###..
.####... #..#.... .#......

#..#.##. .#..###. #.##....
#.####.. #.####.# .#.###..
###.#.#. ..#.#### ##.#..##
#.####.. ..##..## ######.#
##..##.# ...#...# .#.#.#..
...#..#. .#.#.##. .###.###
.#.#.... #.##.#.. .###.##.
###.#... #..#.##. ######..

.#.#.### .##.##.# ..#.##..
.####.## #.#...## #.#..#.#
..#.#..# ..#.#.#. ####.###
#..####. ..#.#.#. ###.###.
#####..# ####...# ##....##
#.##..#. .#...#.. ####...#
.#.###.. ##..##.. ####.##.
...###.. .##...#. ..#..###
"))

  ;; Assert that given the example input our placed and trimmed tiles
  ;; result in the same image given in the problem statement.
  (assert (eql nil (mismatch
           (with-output-to-string (*standard-output*)
             (format-image
              (placement-to-image
               (place-tiles
                (parse-tiles
                 *example-input*)))))
           ".#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###
")))

  (destructuring-bind (image snakes) (find-sea-monsters
                                      (placement-to-image
                                       (place-tiles
                                        (parse-tiles
                                         *example-input*))))
    (assert (image-equal
             snakes
             (parse-image
              '("........................"
                "........................"
                "....................#..."
                "..#....##....##....###.."
                "...#..#..#..#..#..#....."
                "........................"
                "........................"
                "........................"
                "........................"
                "........................"
                "........................"
                "........................"
                "........................"
                "........................"
                "........................"
                "........................"
                "...................#...."
                ".#....##....##....###..."
                "..#..#..#..#..#..#......"
                "........................"
                "........................"
                "........................"
                "........................"
                "........................"))))

    (assert (= 30 (image-bit-count snakes)))
    (assert (= 303 (image-bit-count image))))

  (assert (= 20899048083289 (part-one *example-input*)))
  (assert (= 273 (part-two *example-input*)))

  ;; These are the answers to Part One and Two.
  (assert (= 18262194216271 (part-one *input*)))
  (assert (= 2023 (part-two *input*))))
