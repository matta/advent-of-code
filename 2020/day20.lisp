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

(deftype array2d () '(array * (* *)))

(defun mirror-array2d (a)
  (check-type a array2d)
  "Return a mirror of a two dimensional array. The rows are reversed."
  (let* ((b (make-array (array-dimensions a) :element-type (array-element-type a)))
         (maxi (1- (array-dimension a 0))))
    (loop for j from 0 below (array-dimension a 1) do
      (loop for i from 0 to maxi do
        (setf (aref b i j) (aref a (- maxi i) j))))
    b))

(defun rotate-array2d (a)
  (check-type a array2d)
  (let ((b (make-array (reverse (array-dimensions a))
                       :element-type (array-element-type a)))
        (m (array-dimension a 0))
        (n (array-dimension a 1)))
    (loop for i below m do
      (loop for j below n do
        (setf (aref b (- n 1 j) i)
              (aref a i j))))
    b))

(defun extract-array2d (a start-y start-x dim-y dim-x)
  (check-type a array2d)
  (assert (<= 0 start-y (array-dimension a 0)))
  (assert (<= 0 start-x (array-dimension a 1)))
  (assert (<= 0 (+ start-y dim-y) (array-dimension a 0)))
  (assert (<= 0 (+ start-x dim-x) (array-dimension a 1)))
  (let ((e (make-array (list dim-y dim-x)
                       :element-type (array-element-type a))))
    (loop for y below dim-y do
        (loop for x below dim-x do
          (setf (aref e y x)
                (aref a (+ start-y y) (+ start-x x)))))
    e))

(defun stack-horizontal-array2d (a b)
  (check-type a array2d)
  (assert (= (array-dimension a 0)
             (array-dimension b 0)))
  (let* ((a-ydim (array-dimension a 0))
         (a-xdim (array-dimension a 1))
         (b-xdim (array-dimension b 1))
         (stacked (make-array (list a-ydim (+ a-xdim b-xdim))
                              :element-type (array-element-type a))))
    (loop for y below a-ydim
          do (loop for x below a-xdim
                   do (setf (aref stacked y x)
                            (aref a y x)))
          do (loop for x below b-xdim
                   do (setf (aref stacked y (+ x a-xdim))
                            (aref b y x))))
    stacked))

(defun stack-vertical-array2d (a b)
  (check-type a array2d)
  (assert (= (array-dimension a 1)
             (array-dimension b 1)))
  (let* ((a-xdim (array-dimension a 1))
         (a-ydim (array-dimension a 0))
         (b-ydim (array-dimension b 0))
         (stacked (make-array (list (+ a-ydim b-ydim) a-xdim)
                              :element-type (array-element-type a))))
    (loop for x below a-xdim
          do (loop for y below a-ydim
                   do (setf (aref stacked y x)
                            (aref a y x)))
          do (loop for y below b-ydim
                   do (setf (aref stacked (+ y a-ydim) x)
                            (aref b y x))))
    stacked))

(defun m2d (d0 d1)
  (let ((a (make-array `(,d0 ,d1))))
    (dotimes (i (* d0 d1))
      (setf (row-major-aref a i) i))
    a))

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
    :type array2d
    :initform (error "Must supply :image."))
   (sides
    :type side-array)))

(defmethod initialize-instance :after ((object tile) &key)
  (with-slots (image sides) object
    (destructuring-bind (dim-y dim-x) (array-dimensions image)
      (setf sides
            (vector
             (extract-array2d image 0 0 1 dim-x)          ; top
             (extract-array2d image 0 (1- dim-x) dim-y 1) ; left
             (extract-array2d image (1- dim-y) 0 1 dim-x) ; bottom
             (extract-array2d image 0 0 dim-y 1))))))     ; right

(defmethod tile-dimension ((object tile) index)
  (array-dimension (slot-value object 'image) index))

(defmethod tile-bit ((object tile) m n)
  (aref (slot-value object 'image) m n))

(defmethod tile-side ((object tile) index)
  (aref (slot-value object 'sides) index))

(defmethod print-object ((object tile) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (id) object
      :do (format stream "id: ~d" id))))

(defun format-image (image)
  (check-type image array2d)
  (loop for m below (array-dimension image 0) do
    (loop initially (format t "~&")
          finally (format t "~%")
          for n below (array-dimension image 1) do
            (format t "~c" (ecase (aref image m n)
                                       (1 #\#)
                                       (0 #\.))))))

(defun image-to-string (image)
  (with-output-to-string (*standard-output*)
    (format-image image)))

(defun format-tile (tile)
  (with-slots (id image) tile
    (unless (zerop id)
      (format t "~&Tile: ~d~%" id))
    (loop for m below (array-dimension image 0)
          do (loop initially (format t "~&")
                   for n below (array-dimension image 1)
                   do (format t "~c" (ecase (aref image m n)
                                       (1 #\#)
                                       (0 #\.)))
                   finally (format t "~%")))))

(defun format-placement (tiles)
  (check-type tiles array2d)
  (loop for m below (array-dimension tiles 0) do
    (loop initially (format t "~&")
          for n below (array-dimension tiles 1)
          do (format t " ~D" (tile-id (aref tiles m n)))
          finally (format t "~%")))
  (loop
    for m below (array-dimension tiles 0)
    do (loop
         finally (format t "~%")
         for m2 below (tile-dimension (aref tiles m 0) 0)
         do (loop
              finally (format t "~%")
              for n below (array-dimension tiles 1)
              do (loop
                   initially (format t " ")
                   with tile = (aref tiles m n)
                   for n2 below (tile-dimension tile 1)
                   do (format t "~a" (ecase (tile-bit tile m2 n2)
                                       (1 #\#)
                                       (0 #\.))))))))

(defun tile-equal (tile-a tile-b)
  (and (= (tile-id tile-a)
          (tile-id tile-b))
       (equalp (slot-value tile-a 'sides)
               (slot-value tile-b 'sides))))

(defun rotate-tile (tile)
  (with-slots (id image) tile
    (make-instance 'tile
                   :id id
                   :image (rotate-array2d image))))

(defun mirror-tile (tile)
  (with-slots (id image) tile
    (make-instance 'tile
                   :id id
                   :image (mirror-array2d image))))

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

(defun parse-image (rows)
  (loop with image = (make-array (list (length rows)
                                       (length (elt rows 0)))
                                 :element-type 'bit)
        for y below (length rows)
        for row = (elt rows y) do
          (loop for x below (length row)
                for ch = (elt row x) do
                  (setf (aref image y x)
                        (ecase ch
                          (1 1)
                          (0 0)
                          (#\O 1)     ; "Capital Oh" is used only by tests.
                          (#\# 1)
                          (#\. 0))))
        finally (return image)))

(defun parse-tile (str)
  (destructuring-bind (id &rest rows) (split-lines str)
    (make-instance 'tile :id (parse-tile-id id)
                          :image (parse-image rows))))

(defun parse-tiles (str)
  (mapcar #'parse-tile (split-tiles str)))

(defun permute-image (image)
  (delete-duplicates
   (loop for img in (list (mirror-array2d image) image)
         collect img
         collect (setq img (rotate-array2d img))
         collect (setq img (rotate-array2d img))
         collect (setq img (rotate-array2d img)))
   :test #'equalp))

(defun permute-tile (function tile)
  (flet ((each-rotation (function tile &optional)
           (loop for i below 4
                 for it = tile then (rotate-tile it)
                 do (funcall function it))))
    (each-rotation function (mirror-tile tile))
    (each-rotation function tile)))

(defun permute-tiles (tiles)
  (loop with results
        for tile in tiles
        do (permute-tile
            #'(lambda (permuted)
                (push permuted results))
            tile)
        finally (return (nreverse results))))

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
  (labels ((stack-horizontal (a b)
             (stack-horizontal-array2d a b))
           (stack-vertical (a b)
             (stack-vertical-array2d a b)))
    (reduce
     #'stack-horizontal
     (loop for m below (array-dimension images 0)
           collect (reduce #'stack-vertical
                           (loop for n below (array-dimension images 1)
                                 collect (aref images m n)))))))

(defun copy-into (from to to-y to-x)
  (check-type from array2d)
  (check-type to array2d)
  (assert (<= 0 to-y (+ to-y (array-dimension from 0)) (array-dimension to 0)))
  (assert (<= 0 to-x (+ to-x (array-dimension from 1)) (array-dimension to 1)))
  (loop for y below (array-dimension from 0) do
    (loop for x below (array-dimension from 1) do
      (setf (aref to (+ y to-y) (+ x to-x)) (aref from y x)))))

(defun each-shifted-image (function image dimensions)
  (check-type image array2d)
  (assert (= 2 (length dimensions)))
  (assert (<= (array-dimension image 0) (first dimensions)))
  (assert (<= (array-dimension image 1) (second dimensions)))
  (loop for shift-y to (- (first dimensions) (array-dimension image 0)) do
    (loop for shift-x to (- (second dimensions) (array-dimension image 1)) do
      (let ((shifted (make-array dimensions
                                 :element-type 'bit
                                 :initial-element 0)))
        (copy-into image shifted shift-y shift-x)
        (funcall function shifted)))))

(defun find-subimage-mask (mask image)
  (let ((found))
    (each-shifted-image #'(lambda (shifted)
                            ;; (format t "~&shifted~%")
                            ;; (format-image shifted)
                            (let ((masked (bit-and shifted image)))
                              ;; (format t "~&masked~%")
                              ;; (format-image masked)
                              (assert (equalp shifted (mirror-array2d (mirror-array2d shifted))))
                              (when (equalp shifted masked)
                                (break)
                                (push shifted found))))
                        mask
                        (array-dimensions image))
    (when found
      (reduce #'bit-ior found))))

(defun trim-image (image)
  (extract-array2d image
                   1 1
                   (- (array-dimension image 0) 2)
                   (- (array-dimension image 1) 2)))


(defun pad-image (image)
  (check-type image array2d)
  (let ((padded (make-array (list (+ 2 (array-dimension image 0))
                                  (+ 2 (array-dimension image 1)))
                            :element-type 'bit
                            :initial-element 0)))
    (copy-into image padded 1 1)
    padded))

(defun trim-tile-grid (tile-grid)
  (let ((image-grid (make-array (array-dimensions tile-grid))))
    (loop for m below (array-dimension tile-grid 0) do
      (loop for n below (array-dimension tile-grid 1) do
        (setf (aref image-grid m n)
              (trim-image (slot-value (aref tile-grid m n)
                                      'image)))))
    image-grid))

(defun placement-to-image (placement)
  (grid-to-image (trim-tile-grid placement)))

(defun find-sea-monster (image)
  (let* ((sea-monster (sea-monster))
         (images (permute-image image)))
    (format t "~&Sea Monster~%")
    (format-image sea-monster)
    (loop for i below (length images)
          do (format t "~&~%Image ~D~%" i)
          do (format-image (elt images i))
          finally (format t "~&"))
    (loop for image in images
          for mask = (find-subimage-mask sea-monster image)
          if mask do
            (return mask))))

(defun test ()
  ;; parse-image supports row lists and row arrays.  Rows themselves
  ;; may be arrays/lists of 0 or . for zero and 1 or # for one.
  (let ((result #2A((1 0 1) (0 1 0))))
    (dolist (input '(((#\# #\. #\#) (#\. #\# #\.))
                     ("#.#" ".#.")
                     (#*101 #*010)
                     #(#*101 #*010)))
      (assert (equalp result (parse-image input)))))

  ;; permute-image should produce the eight possible variants of a given
  ;; image.
  (assert (equalp (permute-image (parse-image #("###"
                                                "#.."
                                                "...")))
                  '(#2A((0 0 0) (1 0 0) (1 1 1))
                    #2A((0 0 1) (0 0 1) (0 1 1))
                    #2A((1 1 1) (0 0 1) (0 0 0))
                    #2A((1 1 0) (1 0 0) (1 0 0))
                    #2A((1 1 1) (1 0 0) (0 0 0))
                    #2A((1 0 0) (1 0 0) (1 1 0))
                    #2A((0 0 0) (0 0 1) (1 1 1))
                    #2A((0 1 1) (0 0 1) (0 0 1)))))

  ;; pad-image pads images with an edge of blank pixels.
  (assert (equalp (parse-image '("..."
                                 ".#."
                                 "..."))
                  (pad-image
                   (parse-image '("#")))))


  ;; trim-image removes the outermost pixels.
  (assert (equalp (trim-image
                   (parse-image '("..."
                                  ".#."
                                  "...")))
                  (parse-image '("#"))))

  (let ((shifted))
    (each-shifted-image #'(lambda (img)
                            (push img shifted))
                        (parse-image #("#."
                                       ".#"))
                        '(3 3))
    (setq shifted (reverse shifted))
    (assert (= 4 (length shifted)))
    (assert (equalp shifted
                    (list (parse-image #("#.."
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
                                         "..#"))))))

  ;; grid-to-image stacks the images as appropriate.
  (assert
   (equalp
    (parse-image '("........"
                   "..#..#.."
                   ".#....#."
                   "........"
                   "........"
                   ".#....#."
                   "..#..#.."
                   "........"))
    (grid-to-image
     (make-array
      '(2 2)
      :initial-contents
      `((,(parse-image #("...."
                         "..#."
                         ".#.."
                         "...."))
          ,(parse-image #("...."
                          ".#.."
                          "..#."
                          "....")))
        (,(parse-image #("...."
                         ".#.."
                         "..#."
                         "...."))
          ,(parse-image #("...."
                          "..#."
                          ".#.."
                          "...."))))))))

  (assert (= 20899048083289 (part-one *example-input*)))

  ;; This is the answer to Part One.
  (assert (= 18262194216271 (part-one *input*)))

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
           :test #'equalp))

  ;; The placement found for the example input is identical to that given
  ;; in the problem statement.
  (assert (string=
           (with-output-to-string (*standard-output*)
             (format-placement (place-tiles
                                (parse-tiles
                                 *example-input*))))
           " 1951 2311 3079
 2729 1427 2473
 2971 1489 1171
 #...##.#.. ..###..### #.#.#####.
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

  ;; Assert that the image with the snakes in it, given in the problem
  ;; statement, is produced with our placement code when given
  ;; *EXAMPLE-INPUT*.
  (assert (position
           (parse-image '(".####...#####..#...###.."
                          "#####..#..#.#.####..#.#."
                          ".#.#...#.###...#.##.O#.."
                          "#.O.##.OO#.#.OO.##.OOO##"
                          "..#O.#O#.O##O..O.#O##.##"
                          "...#.#..##.##...#..#..##"
                          "#.##.#..#.#..#..##.#.#.."
                          ".###.##.....#...###.#..."
                          "#.####.#.#....##.#..#.#."
                          "##...#..#....#..#...####"
                          "..#.##...###..#.#####..#"
                          "....#.##.#.#####....#..."
                          "..##.##.###.....#.##..#."
                          "#...#...###..####....##."
                          ".#.##...#.##.#.#.###...#"
                          "#.###.#..####...##..#..."
                          "#.###...#.##...#.##O###."
                          ".O##.#OO.###OO##..OOO##."
                          "..O#.O..O..O.#O##O##.###"
                          "#.#..##.########..#..##."
                          "#.#####..#.#...##..#...."
                          "#....##..#.#########..##"
                          "#...#.....#..##...###.##"
                          "#..###....##.#...##.##.#"))
           (permute-image
            (placement-to-image
             (place-tiles
              (parse-tiles *example-input*))))))
  )
