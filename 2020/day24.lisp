;;;; See https://www.redblobgames.com/grids/hexagons/
;;;; See https://www.redblobgames.com/grids/hexagons/implementation.html
;;;; See https://github.com/Ramarren/cl-geometry/blob/master/basic-point.lisp

(defpackage :day24 (:use #:common-lisp))
(in-package :day24)

(deftype cube () `(simple-array fixnum (,3)))

(defun make-cube (x y z)
  (make-array 3 :element-type 'fixnum
                :initial-contents (list z y x)))

(defun make-cube-from-list (list)
  (make-array 3 :element-type 'fixnum
                :initial-contents list))

(defun check-cube-valid-p (value)
  (check-type value cube))

(defun cube-equal-p (cube1 cube2)
  (equalp cube1 cube2))

(defun cube-add (cube1 cube2)
  (make-cube-from-list (loop for d1 across cube1
                            for d2 across cube2
                             collect (+ d1 d2))))

(defun cube-to-offset (cube)
  "Converts a cube coordinate to offset coordinates."
  (let* ((z (aref cube 0))
         (x (aref cube 2))
         (col (+ x (truncate (/ (- z
                                   (if (oddp z) 1 0))
                                2))))
         (row z))
    (list col row)))

;; (gethash)
;; (make-hash-table)
;; (sxhash)

;; route-list -> route (\n route)*
;; route -> step (step)*
;; step -> e | se | sw | w | nw | ne

(defun parse-stream (input-stream)
  (labels
      ((peek () (peek-char nil input-stream nil nil))
       (consume () (read-char input-stream))
       (parse-route-list ()
         (loop collect (parse-route)
               while (eql (peek) #\Newline)
               until (null (progn (consume)
                                  (peek)))
               finally (assert (null (peek)) ()
                               "Premature end of parse at ~S" (peek))))
       (parse-route ()
         (loop collect (parse-step)
               while (member (peek) '(#\n #\s #\e #\w))))
       (parse-step ()
         (ecase (peek)
           (#\e (consume) 'east)
           (#\s (consume) (ecase (peek)
                            (#\e (consume) 'southeast)
                            (#\w (consume) 'southwest)))
           (#\w (consume) 'west)
           (#\n (consume) (ecase (peek)
                            (#\e (consume) 'northeast)
                            (#\w (consume) 'northwest))))))
    (parse-route-list)))

(defun parse-string (str)
  (with-input-from-string (input-stream str)
    (parse-stream input-stream)))

(defparameter *simple-input*
  "e
ee
nw
")

(defparameter *example-input*
  "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew
")

(defparameter *input* "enesenwwwsenewswsewenwwnwwnwnwswse
swneswseesweswneswwwwseewnewswsw
neneenenenenwneseneesenewenenwnenenesw
weeswnenenwneneeesweneswenwneene
nwnwnenwnwwnwnwnwnwwsewnwswnwwnwnwnw
nwwwnwnwnenenwewnwseewnwswnwwnww
senwweneewewswweswsenwnesenwwnwswnw
nwnwnenenenenwnesenenenwneswnwnweeswwsene
wswnwswneneswswswswswnesweseswwswnwswsw
neeeeeweseesesweeneweneesw
wwenewwwwwwnwnwwwwwwwwse
swneswswseseswswswsw
seswswsewseesesese
nenwneneneewseneneneenenenenenesenenew
swswwwnewwsewwwswwwswswwnesew
wwswwwwwwwwwwwswewwwwe
senewnwwnwswnwswnwewnenwenwewsenwnwse
wnwnwenwwnwnwwnewnwswsenwnwnwnwwne
nenwneswnwnwswnwnwesewwnwnwnwsenwnene
wsewneeseneseseseswnesesenwswenwswnwe
nesewnwwwswnweneenwswwesenesw
swsesenwswsesesewswswneseseeswnesesesewsw
neneswswwswswwswseswswewwswswswsew
eeeneeneneneneesweenewneneeee
seeenesesweseeeewwneeseeseee
nweeweeneeswenewe
seswwnwseeswseseswswe
wneneeswnesenwseneeneneneenwneneneenwne
newnenesenenenenenewneswnenenenenenenenese
wswswswswwswwwwweswnwswsewswswsw
eeeeeenesenwewseeneeneneweee
sewnwwnwnwwwnwnwnwnwswwwewnwww
sewesenwswwneswnwnwnwsweseseeswwse
nwwewnwweswswwwwewnwnwnwwnwwswe
swwwswnwswswenwneswswseswsweswseswsene
eneeenwswseseeeeeesenwenwwsweee
swseenwseesewnwnewnwneeeswswesenee
swwsesenwnwnwwwwnewenwnwewwwwsw
seeeeeeeeswnweeenwnwnewswesw
seenwswnenenwnewnwnwnwnwe
esenenweeswseeeseeweseseeeee
swswswswseswswswswnenw
neneseswsenenewneenenewwnenenenenenenene
wnewwenweswswwwswseswswwwnesene
seneneneweenwnwseeeweeseseeeene
neenwsweneneeeswwnwwneeenewnene
seseneswwneswseswwsesenwseseseswsesese
wweeeeeeswsenweeneeee
newnenenwneseneneeneeneneseneneeneene
wewnwnwwwwwwsenewnwwnwnwwwse
weseeseseswnwsewseneneeeesewwe
swswswswswswswswswswswwneswswwwswswe
swsenwneeseswseswswswswseseneseswswswsw
seswnenwwswswswneswsewwnewseswswwwsw
swswswneswswseswnwswseswseswswseneseswsw
wwwwnewesesewswwwnewwwne
enweeneseneeeeswnwesw
eesesenwseseeseseesesese
nwswseenesewwseesenee
wwswswswwswswwneesw
newwseswwnewswwwwswnwewnewwsww
nesenenenewenesenenenenwnwswnenwnwnenene
swnwswswneswswneswswswswseswswseswsweswwsw
swswswswswswneseswswseswnwswswswsweswsw
nwwwwswwwsewwwwewnwewswwww
neneeneeewneeneeeneenene
swnwnwnwneswwwseeenewnewswsenwwnww
nenenenwnenwneswnenwnenenenenene
nwweenwesweeeeeeseeeeewnenese
weeeneeseeeseeeseeeewe
wwwwwnwesewwwswewwwwenww
seneseseseseseneswsesesesewsewsesesese
eenwneneseeeeeesweenweeeenee
neseneseseseseseeseswseeeeseewee
nwswnweeeeseeseneeneneswswneneee
sesesewseseswesesesewseeneeseesese
seswnewwswewnwswneneswwnwnwnew
wneseneeeewsesewneneneenenwswnwne
wnwnenwnwnwnwsenenwnesenwnwnwnwnwnwnwnwwnw
wwnewnwwswweswewswseeewenwsw
eneeneseeneneneewneweeeneeeneew
nwwnwnwwnwnwenenwnwnwenwnenwnenwnenw
nwsenwnwenwnwswsenwsenwnwnwnwnwnwswenwnw
eeseseeeeeseseeeneesenwseeswe
ewenewneeneswewneseneswsewswwese
nwwwwswwswswwse
newnwnwnwwwwnwnwswsewewnwwwwnwnw
seswswnwwseseseswseesw
sewseweswwwwwswwwwwswwwwnwne
senenenenwnwnwnwnwnwnwnwnw
swswswwwwswswwnwwwswwesewwww
seeneseseneseesesesewsesesesesenwseswsw
wnenwnwsewwwnwnwwwwwsewwnwnew
swswswswswneswseswswswseseseswswnwsesese
neneneneneeneenenesenenwnenewnesenenee
wseseswseneeseswneseswseswswwwseswne
nenenenenenenwnenenenesenesewswneneenwe
seeseseseeseneesesenweeewseewsese
neeewneneneneseneenenenenenenesenwne
nenewnweneweseenwnwsesewnwswnwnwnw
sewnwnesenenwneswwnenwnwnwsenweneswe
senwnwnenwswnwnwnwnenwenenwnwnenenesenw
wnwwwwwsewnwwwewwwweseww
swnwnewnwnwsenwsenwnwnwnenwneswwwnwenenw
wnwseswnewwweewneneeseseesenww
wseseseseeseseseseseesesesenwswswswnw
neneneneneneneseneswnenenwneneneneneenewne
neswswswswswswsesesesesewsewneswsenesw
nenwneswnwweseeenwnesweneeneeese
seesesesesesweeseseseesweenweseenwse
seeswseseseswseseseswswswnenwswswseswsw
swneswswseswswswwswswnwswseswswswswseneswsw
nwseseesesewneeseseeeeeseewenesese
nenenenewneneswneswneneswswneneneneene
nwswnwnenwnwnwnwswnewnwnwnwenwnwnwnww
nwwnwnwnwnenwnwnenwsenenwnenwnwnwnwnw
senenenwnwewwwneeneewnenwsew
nenenenwneneneneswneneneeswenenenwnenw
wwneweewseswseseneneeseeeseese
neneneeswnenenewneneneneneneswenenene
eeeeewseeeneneneeeeeweenwsw
nwenenwnwswwsenwnwswneswnwnwnenwenenwne
eswwswneswswswswswnwewswswwswswwswswsw
enwwwwnwnwnwwewwnwnwnwnwseeww
seseseneeseseseseneseseewwseesesesese
nenenweneswnewenw
nenenenwneneneseswnwnwsenwenwewnenenwnw
nenenenwnwnwenwnwswnwnwwnwnwnwnwnwsenw
neeneneneeneneneneneneenesw
neeeneeeswnwnenwsewswswnenwsenwnee
wenwwwswenewwwsewswswnewswwew
seseeneeseseseneseseesesewsesewsesesese
swewnwwsenwnwwseewwnwwwnewwnw
wesesweeneeenewnwsweenenwneeesw
nwnwswnwnwnwnwnwenenwnwswnwnwnwnwnwenenw
nwsenwwswnenwneeesenewenwwwnw
ewseeeeenweseseseeeewseeseese
wnwnwsewwnwneswwnwwnwnwwnwwwnwnenw
swswsenwwswwenenenewseswswewswnesw
wnwswsesesesweeswseswswswswswseswswsw
eneeswnwwnwnwsweenwenwnwnewnwswswnwne
seseswswseseswseswsenesesewsenenwsesww
eeeneswnwnwnwneneseneseneenewnenwsee
enweseesweswneeweseenwneeew
newneneneneeneneswsenenewnenenene
eneswwneneenwnenesenewwnenenesenene
swswswswswsesenwswswswswsw
eeeneewewneeeee
swswsesenwswsenwswseswsweswseswnwseswswenw
sweneseswswesenenenwnwwnwnwneswe
wnwnwnwwwwsweneswnwnwnwewwnwww
seseswswswsesenwwseeseswswswswweese
seseeseseseseesesesenwnenwsenwsesesesew
wnwnwswnwwwwwwsewwwnenwwwwe
wnwwsenwneewswnwsenwnenwwnwesenwnewsw
swnenwnenenwnwwnwnesenenwnwnenwnenenenwnw
wwewwwwnwswwwnewwwswwwwnw
neneeeweesesenweneneswneeeewne
swswswseneneswwseseseneseseseswswswswswsw
nwnwnenwswwnwwwwnwwwwsew
swswswswseswsesesenwswsewseseswswesesw
nenwswnwnenwswnenwnwnenwnwnwsenwnwneenw
wseseswswswneseswseseswswsenwswswne
swnesesewseneseswswnwswswswswnwswseswswswsw
wnwnwnwnenwsenwneenwswnwnwnwenwsenwswnw
enenenewnenenwnenenenwnenenenenw
enenwnenwsewwwsweenenewnwnenwene
swwswsewnewswneswswwwswswwswwswwsw
neneswseeneeswewenwwswswweesese
swnwseswsenwsenweewenwsweeneeese
nwenwnwnwnwswnwnenwnwnwnwnwnwnwnwwsenw
seseseseeseseseeeeewesenesesewe
eswsenwnwneswwswwswnwwwwseeswsenwe
enenewnesenewnwnwnwswnwnwnenesenenwnenw
wneneneeneeeeweenewnenesweesene
enewnweneneeneeeswneneeeeenene
nwswsesenwnenwnwwnwnweswswnwnwneeew
eseeewneeneeeeeneeeeeseewene
sewnewwwwwwwsewwwwwwwnwwe
newnenesenweneneneweneenene
swseswswswswswswswswewswswswswswnwneswswsw
eseneneseseeseseeeeseeeeeswsewse
enwneenenewneeneneneswnenenenenenene
nenenwnenenwseneneenwnwnenwnweswnwnww
nwnwenwnwsenwnwnwnwnwwwse
nweswswwswenenweeeseseseeeeeee
nwnenwswnwwnwenwsewwnwneswnwnwnwnwsee
seeseeseseneewswweseseswswwwswswsw
nwnwnesenwenwnwnwnwnwnwwnwneswnwswwnwnw
seseseseswseswesesesesesesenesewswsenw
swswwswseseseseswsewseseneseseswneswswsw
nwnwnwnenwswnwnwnenesenenwnwnenwnwnwnenw
swnewwnwneenwnwenwsenwnwnwsewnwenw
eeenwwweeeewseneeswesenese
swenewneseneswneenwnesenenenwnenewene
nwnenwwwwwnewwwwwwwsesenwwww
seseswneseseseeenewwsese
enwneeeswenewneeeeneeseneeee
seseeseneseeeeeeswwnenwseswsesese
nwnesesenenenwnwnenwnwnenenwnewnwnwnenw
wwswnwnwewewswneseeswswswseneswsw
nwsenwneneneneneneeseewnenenenenesenwse
nesewswswneneswneswseswswnewswswsenese
swswwsesweswnwseseswseseswseseseseswsw
senenwswenwnwnwwnenwsenwnwnwnenwewnwwnw
nenenenwnwneswswneswnenenenenenenenenene
enweneswneneneeswneneeewneenenee
nwnwswnwwnwseesenenwwswwnewwsenwnwnw
neswseseswneeswneneswswneswenwwswwwe
swwneeneeneswwewnwneseeenwnenwnewne
wswwwseenwnewneswwnwwwnwseew
seewwnenenwneswwnenwneseneenenenene
swwsenwswsweseesenwwwswswwnewnesenew
neswswswnenesenenenw
nwwsewwwnwwnewnwwnenwwseswnenwswwnw
nenwneseeewneeeeseneesweneenwsee
seswseswseswnwnwswswseswsweseswswswswse
swwswneswswswswswswneswsweswswswswwswsesw
senesewseseseeewseweseseseeseesese
swnesenenenwneneneswnwnwne
wsesewwenenwsesewneneseseseswsesese
newseswwwswwwnwswweswwewswwww
nweneeneneeswnwwenenesweneseneene
esweneenweeeeseweeeneeewew
neswnweswswnenweseswswnesewswsenwsese
nwnwnwnwnwwenwwwnwnw
swswseseswswswswswswswswswsweswneswswnwsw
neneswnenwnenenenenenwnenwne
seeseseneseseswseseseseswsesesenwsesesese
nwnenwnwseenwnewewswsewnwwswswnew
wseesesweseseeneeenwseeeeeenee
swwwsewesewnewnweenwswswwwwwsw
eeneseseeseseseseswseewse
eeeneseeeeeeswseeenewseeee
swseswswsewenenwnwwnweswne
ewwnenwswnwnenwnwnwnenenwnwenenenwnw
neswswswswswnwswwswsewsw
seseeseswswewsewsenwseseneewsewneee
seswseswenwwnwswwswewnweseswnwnw
sesesenesesesewswseseswneseswsesesesesese
neneneneneseenenewneeneneswwnesenenew
nweeewneenwswneneswnewne
nwwewwswwwseswwswwne
nwnenwnwnwnwsewnwnwwnwewnwnwnwnwwnwnw
seneneneneenenwnewneneneswneenenwswnenese
swseswnwseswswseseseswnwnewseswswswswswsesw
nwnenenwnwnenwnenwnwseswneneenwnesenwne
nenenesweeenenenenewne
eewneeeweeeeseeeneneewene
swsweswswswswswswswswneswnwseswnwwseswsw
wwswwwnwnwewseweswnwnesenwwwew
seeneenenenesenenesweweenenenenwnene
newewseewnwwnwwwsewsenewswswe
sweeseweseseeeseeenesesesenwese
neswewwwseswwwswsww
wewnwnwnwnwnwwwnwwswesenwnwenwsee
seswsewsewseswswenenewseenwnwnwswse
swswseswseswseswswswnwswswnwsenesesw
nwnwsenenenwsewnenenenwsenwnesesenwnwnwnw
swnenwwwwwseswwneewswswwwnesene
seseseswswsesenesesesesesw
eneenwneenenenewswneswnesweee
nwnwneswnenenwsenenenenenenenwnenewnene
seswnewswswswnwswsenwwsweswswnwswesee
swswswswswwswswswewswneswswseswswswneswnw
swswswswswseseswsenwseneesesesesewwsese
seneeesweeeeeeweeeee
senweseeeenweeeenwenwsweeswese
swswneswseewswswnwswswwsenweswswswesw
ewneeeeeneweeeeeeweseesesese
nwseswseneeseeseesweeweseesesee
neeneswnenenewnwnenwnenenenwswneenewne
eswwswswnwnwseeenwweswnwnenwswswse
wwswnewwswnwwwswwneneeswwseswne
neneneneseneneswneneeneeewne
wwwnwwnewsenweseewwwewwnew
neswwsweewneswsenewsesewwsenwsesese
seswwsweenwweseneseneseneeenwwe
senwsesweseseswnwseeseswsenesesenenewsw
swseswswswswnenwswswwswswswswswwswenesw
nesenwnwnwnenwnwnwnenwnewnwnwnwnwnesenw
seswswswwswswwswswswswneswswenewnwswsw
nwnwwnwnwnwnenwenwwwenwswwswnwnwnwnww
neseenesenwnwsenwwneesewnewwswenwnenw
wnenwnwnenenenwnenesenwnenenwnenwnenwe
nwnwnwnwnenwnwnesenwnenwnwnenewnwnenwswe
enwswseeseseseewseeswnweeseeeee
eeeeeesenwseseeee
swsesweweswwswswswswnwnwwswswsw
neseneneeeenwweneenenenene
swwsenwnenenwnesesewneeswwswseswnwswswse
seeenweesesweseeeweeeseeswnw
sewsesesesesenesweesenwswsesw
nwnenwwnwnwswnwnenwnwwnwnenwnwnwnwsenwe
neenenwneeneswnenwneswneeneeneeneene
seseeeweseseeeeseese
nenwnenwnenenenenwswneneneenwnenenenw
wwwwswswwsweswnewwnewsewwwne
wwnenwnwsewnwnenwsenwnwnwnwnwwwnwnwsw
seseseswswsesenwwsenesesesesesesene
nwseswsesenwwswnenesenenwsw
swswsweswswswseweswnwswswneseswswswnwsw
enweesweneeneeneneeeeeneeewe
seswseseseseswsesewseseseswswsenwesese
seswsesesesenwnewsesesesesesesesesesenwsese
swswswnwswswswseswnweswswswsw
newnwwswwneeneneseeneswnenenenwnenwnw
swwswnwwswswnwswwewnwsweswswswseesw
sweswnwswenwwswswwwwsweswswswswswsw
nenenwnwsenesenenwnwnwnenenenenenwnwswnwne
wnwseswwewwnewwswnwnwwswewsesww
seseneseseswsesewswsesesesene
wwnwwwewwwewwwnwwwwwnwsww
wneeneesenwseeeeeweeesweswene
senesenwsweseeseesewseseenesewwsee
nwnwnwsenwnwswesenwenwnwnwnwnwnwnwswnwnwnw
eeweeeeeesweneeeseeneeneee
wwsenweswsewswneswswnweneswneenww
eeneneneeneeeeenewswneneeseenee
swwseseneswseseweseseswneesenesesese
eeeeneneneneneweeee
swsenesewseswsenwnenwsesweswsenwswswswnw
nwnwswsweswnenwnwnwnenenwnwnwnwnwsenenw
swsesweseseseweseeseswnwsesesenwseswsesw
eneeseneswswsweeweewnewne
neneeseenenenewenenenewnenenwswesw
sweeswwewsweewnwenwnwnesesenwesw
swswswswswswneswswswsw
eseseeneeeseewenwweeeeswsesese
swswwnwswnwwwswewweswwwwswswwe
wwewswwnwwwwwswwwwwweww
enenwnwwnwwwwsesese
wwwwnwswwwswswwwewwwew
wnenwneeswneneneseneewsw
sweswnenenwewsesenwsenw
wsewwenwnwnwwwnwwwnwnwwwnwww
eseeeeeeseeeseneeneeewwwee
nwwnwewsewnenenwwnwswwnwwwswnwse
wwswwwwweswwswwswswewwwneww
swnewnwnwnewseswewnesweswswesesw
nwnwnwswnwnenwnwnwnwnwnwewnwenwnwnwnw
swseweswnesewneesenwnesenwwseesew
wenweseesenwseswsesweswnenwneenwse
seeneneneeeenweneene
nwnwnenwneenwneswnenwnwnwnwswnwnwnwnwnw
esesesweswneneneswnwnwnenwneneseww
nwnwwenwnwnenwnenweswwenwnwnwnwnenesw
senewsewswswseseneswseseseseswsesesese
nwnwnwsesenenwneneswnwnwewnesewnenese
neesesewseseneseseseeseeseseesewsese
swnewneswwswswwwnewsesesenwswswnwsew
neseseswswwseseeswsesesesesesewseswse
swsweswseswswneseeswswswnwnwswwwswneswnw
seeeeseswsesesesesenwsesesee
nenenwnwnwnwnwswsenwnwnwne
eswsweswswsweswseswswwnewswwswswswsw
seneswneswswseswswseswswseswswsenenwswswsw
eswswseswnwnwsesewseseswseneeswswnesww
seseseesesenwsesesenesewseseseseseesesew
swswswswnweswswnewswswswswswwneswwswsw
neeswnenenesweeneneeneneeneenenewne
neswwswnwnwnwwseseneneewwsesewsew
eseseswwwseenesenesesenenwwnwseseswee
swswswseseseseswwswneeswswweswnwsesene
neneneneenesenwwweneswnwswwnwswnenwne
neeeneeeseeneswnenenenenwewnee
nwwnwewsenwnwnwenwesenwneewwnwsw
seswewnwwnwwswnwsenwnwswseenesweene
seswseneseswswseseseweesewwswwnesw
neseeneneneswneeneenenenewneneneenw
nwnwsewnwsenesenwwnewenw
senwneneswewnenenesenenenwewwnene
nenwenwwswwwwwwwwwewwwwse
sesenwsesesenwseesesenwnwswswsenewese
eeenenenwnwsweeeeeeeeeeneneswe
swwwseswwseswswwswwneswswwwnwsww
nwnwwnwwnwnesewnenwnwwwwnenwwwesese
seswswnwswseseseswseswneseseseewswsese
seswswneewneneeswswnwswwwswswnweswse
nwnwnwnwwnwsenwnewnwnwnwneenenenwnwnesw
nenwnwnwnwwnwesenwnwnwnwnwsewwnw
neswwnesenwneswwenwnwsenwnweeswsese
wwnwnwsewnenwwswwnwwwnwwnwnwwew
sewswseseneswseseswswneswseneseseswsesw
swwwnwwwswwswwwwwsewwwnwesww
seneswswwswwnwenwnwwswseeswwwwwwsw
neneswneneswnenesenenenewnenwneneneenw
nwswseseneseseswswswnewswseseseweswnesenw
nenenenenwneseneeneneneneneeswnenenwnee
swseneeeeeeeweneswneweeswwnw
nenenwneeneswneseenewnwswwnwenenenw
senenwnwwnwsewsewnesenenwwnewsewww
swweeenenenenewseeeeewsenwnee
nwneeseseseseseseseseseswse
eeeeweeeeweeneeesweneee
swswseseesesesesesewseseseseneswsewnesesw
eeenwseeeswnweeeeeeeeeeeesw
swswswswsweewswswswwsweswseswswnwnww
sesewsenwseeseeseeeenwnweswnesesesw
seseesesesesewseseeswseeseneesesenw
neeenwnenwsweneneseeenee
seenewesenwseseeseseseseee
nwswswsweswswwwwwswswswwswsw
wwwwnewwswnewwwnewwseswseww
eeseseseseseseseseesesesesenwenw
wwewwwwwwwnwwwww
swswswwswnewswneswswseswwswswwwswsw
wswneswswswwwwneswwseswswwswwswswsesw
nwnenwnenwnenenenewneneneseenwsenwswnwne
wnweswenwneeswswweseneseneeeenw
wnwneneneenenwnenenenene
newswwswweeswseseswswwseneswwnew
wsweneeseseseseseseseeseeseseeswene
sesenwseswseseseswseewnwseseswsese
seswnwnwneesenwseneewswnenwee
nwnwnwnwsenwnwnesewwnenenenenenwnenwnwnenw
senewesweeneswewsenwe
sewewswswwwwswnwwwnewwwwwswsww
eeneeeeeneeseneeeneneneeswnwnw
swseswwsewswseseswswseneswswswseseswneswsw
nenenenwneneseneneneneneneeeeneneswnwne
wnwsenwnwnwwsewnwnwnwnwnwewswnenenw
nweeeeeneesweneeeeneeneeswesw
sesewwnewwwwwnwwwnewwseeww
nwswswnwswneswwwnwswswsweeswnweeneee
wwwwwwwwswwwwwsewwnenewww
nenwsenwseeswneswseseswswenwswswseswsww
swswwswnwswwswswwswswwswwwewwnwew
wnwnwnwnewseswwewnwwwnwnwwnwsew
weseeseseseenenewseesewswsesesese
nenenenwnwneswnwneneseneneenwnenwnenew
nwseeeeswnewsewseeeeesesewesee
wwnwwewwwwnwenwnwwwwswnwwww
nwnwnwswnwnwnwenwnwnwnwnwnwnenwnwnwnw
wseswweseseneswnwneseswsewswswseese
neewseeesweeneenee
enwseswswswseseswswswswwswswsenwnwe
sewwnwwewwsewwwwwenwsewwwne
wewswswwnwnenwswneeswnwnesesenwesw
swswnenwneswswswswswswswswwswseneswswswse
wswwswswwswneweswewnwswswwsewwe
wnwwnwwsewswwwwwsenwwwswnenwswnee
seseeswenenesesewsewswneneseseesenwse
sesesesesesesenweneseseseseneswsewswsese
wnwnenenesweneeweneseeeseeene
sewnwwnwnwwnwwwwnwnwwsenwneww
wwswswwnewneseswnewwsenewswwwsew
nenesewneswnwenwwnesw
nweeeeeeseeesenwneswneeweene
nwnwwnwnwnwwnwnwwnwnwwwwew
seseseseseseseseswsenwseswsesesenesesese
seeswnwswnwnwseswsenesesweswse
seseeseseseseeseseenwsewseseswnenesewse
wweswnwwwnwsewwnwnwwnewnwwnwnwenw
neneswswswswswswwswneeswwswseseswswswse
sewnwseseswnwswseswsesenwseswseneseseswe
seseseseseeneseewseswneseeseseseseese
ewswseswnewswwwswwwswewwnew
swwewwswnesenesenwwnewnw
seseneseswsesenwseseswwnesesesewse
newswwswwswswswwswwnewswswswwsesw
wseneeeeswnwneenweneneweeeeeee
wswsewswswneswsewneneneswwwwswwsw
swwneeneswswwwswwwsenwswwneswnene
seeeeneeeeeswenweeeseseswsese
sesweeenwnewnwnwneeenwswsewsenwsw
seswnwseeseewnwswneenwnwseswneesee
neswnwwnwnwswnwnwnwnwnwnwenwnwnwnwnwnw
swseswswswseswswneswsw
neeswneneeswsewnwnwneswewnenwenee
swswseswswseswseswnwseswswweswswswneswswse
seseewseeseeeswseseseneseeewee
eeeeeseeweswwseene
swswswswswswswswswswseewneswswswswswsw
senwwwnewseesesenwnwenwwswnwnenwnwnw
eeeseswneesweeeeenweeeenee
newswnesweneneweneneneene
nesenwnenenwnwswseswenwwwwsww
senewswswwseswswnwwsw
newwwwsewswwwwswnwsewswsw
nwnwwnwnwsenwnwwnwnwnwnwnenwnwwwenw
swsenwnesesesewswswswswswswswswseswswsw
seneseseswwesewseseneseseeese
wwwsewwwwnwnwswwwwwwwewwsw
neewneeswenenenenenenenenenenewnenew
nwnenwenwnwnwnwwnwnwnwnenewnwneenene
enwwwwwwwwwsenwnwwewww
senwnwnwnwnwenenwwswneenesweewswswne
nesesesesewsewnesenwseswseswsenwsesesw
seseswswswseseenwseseneswnwnw
seseseseseseeeseeweeseseswnwsenesenw
nenewwswseseswseenwenesenwwwesenwswse
nwnwnwnwswwnweswenwnwnwnwnwnwnwenwswnw
wwwenwsesewnwnwnenwsenewsenwenwwse
swsewswnwwwswewnw
enwnwsenwnwenwnenwnwnwnwnwwswnwnwwew
")

(defparameter *cube-east* (make-cube 1 -1 0))
(defparameter *cube-southeast* (make-cube 0 -1 1))
(defparameter *cube-southwest* (make-cube -1 0 1))
(defparameter *cube-west* (make-cube -1 1 0))
(defparameter *cube-northwest* (make-cube 0 1 -1))
(defparameter *cube-northeast* (make-cube 1 0 -1))

(defparameter *neighbors* (list *cube-east*
                                *cube-southeast*
                                *cube-southwest*
                                *cube-west*
                                *cube-northwest*
                                *cube-northeast*))

(defun direction-to-cube (direction)
  (ecase direction
    (east *cube-east*)
    (southeast *cube-southeast*)
    (southwest *cube-southwest*)
    (west *cube-west*)
    (northwest *cube-northwest*)
    (northeast *cube-northeast*)))

(defun route-to-cube (route)
  (reduce #'cube-add 
          (mapcar #'direction-to-cube route)
          :initial-value (make-cube 0 0 0)))

(defun route-list-to-cube (route-list)
  (mapcar #'route-to-cube route-list))

(defun cube-list-to-black-cube (cube-list)
  (reduce #'(lambda (a b)
              (set-exclusive-or a b :test #'cube-equal-p))
          cube-list :key #'list))

(defun part-one (input-string)
  (length
   (cube-list-to-black-cube
    (route-list-to-cube
     (parse-string input-string)))))

(defun make-tiles ()
  (make-hash-table :test #'equalp))

(defun set-tiles (list tiles)
  (loop for tile in list
        do (setf (gethash tile tiles) 'black))
  tiles)

(defparameter *neighbors* (list *cube-east*
                                *cube-southeast*
                                *cube-southwest*
                                *cube-west*
                                *cube-northwest*
                                *cube-northeast*))

(defun map-neighbors (function tile)
  (loop for neighbor in *neighbors*
        do (funcall function (cube-add tile neighbor))))

(defun add-neighbors (tiles)
  (let ((new-tiles (make-tiles)))
    (maphash #'(lambda (tile color)
                 (when (eq color 'black)
                   (map-neighbors
                    #'(lambda (neighbor)
                        (unless (eq 'black (gethash neighbor tiles))
                          (setf (gethash neighbor new-tiles) 'white)))
                    tile)))
             tiles)
    (maphash #'(lambda (tile color)
                 (setf (gethash tile tiles) color))
             new-tiles)))

(defun count-neighbors (tile tiles)
  (let ((count 0))
    (map-neighbors #'(lambda (neighbor)
                           (when (eq 'black (gethash neighbor tiles))
                             (incf count)))
                       tile)
    count))

(defun flip-tile (tile color tiles flipped-tiles)
  (let* ((count (count-neighbors tile tiles))
         (flip-color (cond
                       ((and (eq color 'black)
                             (or (zerop count)
                                 (> count 2)))
                        'white)
                       ((and (eq color 'white)
                             (= count 2))
                        'black)
                       (t nil))))
    (when flip-color
      (setf (gethash tile flipped-tiles) flip-color))))

(defun flip-tiles (tiles)
  (let ((flipped-tiles (make-tiles)))
    (maphash #'(lambda (tile color)
                 (flip-tile tile color tiles flipped-tiles))
             tiles)
    (maphash #'(lambda (tile color)
                 (setf (gethash tile tiles) color))
             flipped-tiles)))

(defun flip (tiles)
  (add-neighbors tiles)
  (flip-tiles tiles))

(defun tiles-to-offsets (tiles)
  (let ((offsets))
    (maphash #'(lambda (tile color)
                 (when (eq color 'black)
                   (push (cube-to-offset tile) offsets)))
             tiles)
    offsets))

(defun offset-bounds (offsets)
  (loop for offset in offsets
        for col = (first offset)
        for row = (second offset)
        minimize col into min-col
        minimize row into min-row
        maximize col into max-col
        maximize row into max-row
        finally (return (list min-col min-row max-col max-row))))

(defun print-offsets (offsets)
  (destructuring-bind (min-col min-row max-col max-row)
      (offset-bounds offsets)
    (loop initially (format t "~&")
          for row from min-row to max-row
          do (when (oddp row) (format t " "))
          do (loop for col from min-col to max-col
                   for present = (member (list col row)
                                         offsets
                                         :test #'equal)
                   for ch = (if present #\@ #\.)
                   do (format t "~C " ch)
                   finally (format t "~%")))))

(defparameter *debug-dump* nil)

(defun part-two-tiles (input days)
  (let ((tiles (set-tiles (cube-list-to-black-cube
                           (route-list-to-cube
                            (parse-string input)))
                          (make-tiles))))
    (flet ((dump (i)
             (when *debug-dump*
               (format t "~&-- ~S ----------------------~%" i)
               (print-offsets (tiles-to-offsets tiles)))))
      (loop initially (dump 0)
            for day from 1 upto days
            do (flip tiles)
            do (dump day))
      tiles)))

(defun delete-white-tiles (tiles)
  (maphash #'(lambda (tile color)
               (when (eq color 'white)
                 (remhash tile tiles)))
           tiles)
  tiles)

(defun part-two (input days)
  (hash-table-count
   (delete-white-tiles
    (part-two-tiles input days))))

(defun test ()
  (assert (= 10 (part-two *example-input* 0)))

  ;; This is the answer to Part One.
  (assert (= 450 (part-two *input* 0)))

  (assert (= 15 (part-two *example-input* 1)))
  (assert (= 12 (part-two *example-input* 2)))
  (assert (= 23 (part-two *example-input* 5)))
  (assert (= 37 (part-two *example-input* 10)))
  (assert (= 2208 (part-two *example-input* 100)))

  ;; This is the answer to Part Two.
  (assert (= 4059 (part-two *input* 100))))
