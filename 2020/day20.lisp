(defpackage #:ma-aoc-2020/day20
  (:use #:common-lisp))
(in-package #:ma-aoc-2020/day20)

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
   (sides
    :documentation "An array of integer holding the color of each side of
the tile -- a sparse and partial representation of the entire matrix in the
problem's input data.  The indices 0..3 are top, right, bottom, and left
respectively.  Each # character in the tile is a bit set in the integer."
    :initarg :sides
    :type side-array
    :initform (error "Must supply :sides."))))

(defun print-tile-color (destination integer)
  (loop for index below +side-bit-length+
        do (format destination "~c" (if (logbitp index integer)
                                        #\#
                                        #\.))))

(defun tile-color-to-string (integer)
  (with-output-to-string (out)
    (print-tile-color out integer)))

(defmethod print-object ((object tile) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (id sides) object
      :do (format stream "id: ~d sides: ~S" id sides))))

(defun check-tile (tile)
  (let ((high-bit (1- +side-bit-length+)))
    (with-slots (sides) tile
      (assert (equal (logbitp 0 (aref sides +top+))
                     (logbitp 0 (aref sides +left+))))

      (assert (equal (logbitp high-bit (aref sides +top+))
                     (logbitp 0 (aref sides +right+))))

      (assert (equal (logbitp high-bit (aref sides +bottom+))
                     (logbitp high-bit (aref sides +right+))))

      (assert (equal (logbitp 0 (aref sides +bottom+))
                     (logbitp high-bit (aref sides +left+))))))
  tile)

(defun print-tile (destination tile)
  (with-slots (id sides) tile
    (format destination "~&Tile: ~d~%" id)
    (print-tile-color destination (aref sides +top+))
    (format destination "~%")
    (let ((left (tile-color-to-string (aref sides +left+)))
          (right (tile-color-to-string (aref sides +right+))))
      (loop for index from 1 below (1- (length left))
            do (format destination "~c        ~c~%"
                       (aref left index)
                       (aref right index))))
    (print-tile-color destination (aref sides +bottom+))
    (format destination "~%")))

(defun tile-equal (tile-a tile-b)
  (and (eql (tile-id tile-a)
            (tile-id tile-b))
       (equalp (slot-value tile-a 'sides)
               (slot-value tile-b 'sides))))

(defun tile-to-string (tile)
  (with-output-to-string (out)
    (print-tile out tile)))

(defun matches-p (side-a tile-a tile-b)
  "Returns true if TILE-A and TILE-B share a common side color when
adjoined along TILE-A's SIDE-A."
  (flet ((opposite-side (side)
           (mod (+ side 2) +sides+)))
    (with-slots ((sides-a sides)) tile-a
      (with-slots ((sides-b sides)) tile-b
        (eql (aref sides-a side-a)
             (aref sides-b (opposite-side side-a)))))))

(defun flip-side (side)
  (let ((flipped 0))
    (dotimes (position +side-bit-length+)
      (setf flipped (logior (ash flipped 1) (logand side 1)))
      (setf side (ash side -1)))
    flipped))

(defun rotate (tile)
  "Returns TILE rotated clockwise one quarter rotation."
  (with-slots (id sides) (check-tile tile)
    (check-tile
     (make-instance 'tile
                    :id id
                    :sides (make-sides
                            (flip-side (aref sides +left+))
                            (aref sides +top+)
                            (flip-side (aref sides +right+))
                            (aref sides +bottom+))))))

(defun flip (tile)
  "Flips a tile on the vertical axis.  Left becomes right, right becomes
left, top and bottom flip orientation."
  (with-slots (id sides) (check-tile tile)
    (check-tile
     (make-instance 'tile
                    :id id
                    :sides (make-sides
                            (flip-side (aref sides +top+))
                            (aref sides +left+)
                            (flip-side (aref sides +bottom+))
                            (aref sides +right+))))))

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

(defun parse-tile-color (str)
  "Parse STR into an integer.  Indices in the string correspond 1:1 to bit
indices in the integer, where #\. is zero and #\# is one."
  (declare (type string str))
  (loop with num = 0
        for bit below (length str)
        do (ecase (aref str bit)
             (#\#
              (setf num (logior num (ash 1 bit))))
             (#\.))
        finally (return num)))

(defun make-sides (top right bottom left)
  (make-array 4 :element-type 'integer
                :initial-contents (list top right bottom left)))

(defun parse-tile-image (rows)
  (let ((top (parse-tile-color (car rows)))
        (bottom (parse-tile-color (car (last rows))))
        left right)
    (loop for row in rows
          collect (aref row 0) into left-list
          collect (aref row (1- (length row))) into right-list
          finally (progn
                    (setf right (parse-tile-color
                                 (coerce right-list 'string)))
                    (setf left (parse-tile-color
                                (coerce left-list 'string)))))
    (make-sides top right bottom left)))

(defun parse-tile (str)
  (destructuring-bind (id &rest rows) (split-lines str)
    (check-tile
     (make-instance 'tile :id (parse-tile-id id)
                          :sides (parse-tile-image rows)))))

(defun parse-tiles (str)
  (mapcar #'parse-tile (split-tiles str)))

(defun permute-tile (function tile)
  (flet ((each-rotation (function tile &optional)
           (loop for i below 4
                 for it = tile then (rotate it)
                 do (funcall function it))))
    (each-rotation function tile)
    (each-rotation function (flip tile))))

(defun permute-tiles (tiles)
  (loop with results
        for tile in tiles
        do (permute-tile
            #'(lambda (permuted)
                (push permuted results))
            tile)
        finally (return results)))

;;;;
;;;; placement - 2d square array sqrt(num tiles)
;;;;
;;;; placed-p - id -> boolean
;;;;   placed: hash tile of id -> tile
;;;;
;;;; fits-p - tile top left -> tile-list
;;;;   sides[2]: hash table of color -> tile
;;;;
;;;;     Returns tiles that match top and left, which may be nil.  Note: if
;;;;     both are nil, return all possible tiles.
;;;;
;;;; compute-placement y x
;;;;   if y x out of bounds
;;;;     return t  ;; base case
;;;;   candidates = fits-p <criteria for y x>
;;;;   for each candidate
;;;;     place candidate
;;;;     when compute-placement y' x'
;;;;       return t
;;;;     unplace candidate
;;;;   return nil
;;;;

(defun all-sides (tiles)
  (let ((all-sides))
    (flet ((push-side (label index tile)
             (push (format nil "~a-~d-~d"
                           label
                           (aref (slot-value tile 'sides) index)
                           (tile-id tile))
                   all-sides)))
      (loop for tile in tiles
            do (push-side "t" +top+ tile)
            do (push-side "r" +right+ tile)
            do (push-side "b" +bottom+ tile)
            do (push-side "l" +left+ tile)))
    all-sides))

(defun make-side-table (tiles)
  "Returns a data structure that makes it easy to find tiles by side index
and color.  See FIND-TILES-BY-SIDE."
  (flet ((make-index (side-index)
           (let ((table (make-hash-table)))
             (loop for tile in tiles
                   do (push tile
                            (gethash (aref (slot-value tile 'sides)
                                           side-index)
                                     table)))
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
               (aref (slot-value (aref placed-array (1- index))
                                 'sides)
                     +right+)))

         (required-top-color (index)
           "Returns the color the top side of the tile at INDEX must be.
I.e. the color of the bottom side of the tile above it.  Returns nil if
INDEX is in the top row."
           (if (< index stride)
               nil
               (aref (slot-value (aref placed-array (- index stride))
                                 'sides)
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
               placed-array
               (let ((candidates (candidate-tiles index)))
                 (dolist (tile candidates)
                   (push-tile tile)
                   (if (place-tiles (1+ index))
                       (return placed-array))
                   (pop-tile))))))

      (let* ((placed (place-tiles 0))
             (corners (list (aref placed 0)
                            (aref placed (- stride 1))
                            (aref placed (- tile-count stride))
                            (aref placed (- tile-count 1)))))
        (values (reduce #'*
                        corners
                        :key #'(lambda (tile) (tile-id tile)))
                corners
                placed)))))

(defun test ()
  (assert (eql #B1000000000 (flip-side 1)))
  (assert (eql #B0001100000 (flip-side #B11000)))

  (let ((tile (parse-tile "Tile 1234:
##........
..........
..........
..........
..........
..........
..........
..........
..........
..........")))
    (assert
     (equal "#<TILE id: 1234 sides: #(3 0 0 1)>"
            (format nil "~a" tile)))
    (assert
     (equal "Tile: 1234
##........
.        .
.        .
.        .
.        .
.        .
.        .
.        .
.        .
..........
"      (with-output-to-string (out) (print-tile out tile))))

    (setf tile (rotate tile))
    (assert
     (equal "#<TILE id: 1234 sides: #(512 3 0 0)>"
            (format nil "~a" tile)))
    (assert
     (equal "Tile: 1234
.........#
.        #
.        .
.        .
.        .
.        .
.        .
.        .
.        .
..........
"
            (tile-to-string tile)))

    (setf tile (rotate tile))
    (assert
     (equal "#<TILE id: 1234 sides: #(0 512 768 0)>"
            (format nil "~a" tile)))
    (assert
     (equal "Tile: 1234
..........
.        .
.        .
.        .
.        .
.        .
.        .
.        .
.        .
........##
"
            (tile-to-string tile)))

    (setf tile (rotate tile))
    (assert
     (equal "#<TILE id: 1234 sides: #(0 0 1 768)>"
            (format nil "~a" tile)))
    (assert
     (equal "Tile: 1234
..........
.        .
.        .
.        .
.        .
.        .
.        .
.        .
#        .
#.........
"
            (tile-to-string tile)))


    (setf tile (rotate tile))
    (assert
     (equal "#<TILE id: 1234 sides: #(3 0 0 1)>"
            (format nil "~a" tile)))
    (assert
     (equal "Tile: 1234
##........
.        .
.        .
.        .
.        .
.        .
.        .
.        .
.        .
..........
"
            (tile-to-string tile)))

    (setf tile (flip tile))
    (assert
     (equal "#<TILE id: 1234 sides: #(768 1 0 0)>"
            (format nil "~a" tile)))
    (assert
     (equal "Tile: 1234
........##
.        .
.        .
.        .
.        .
.        .
.        .
.        .
.        .
..........
"
            (tile-to-string tile))))

  (assert (eql 20899048083289 (place-tiles (parse-tiles *example-input*))))

  ;; This is the answer to Part One.
  (assert (eql 18262194216271 (place-tiles (parse-tiles *input*)))))
