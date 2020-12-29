(defpackage :day19 (:use #:common-lisp))
(in-package :day19)

(defparameter *example-input*
  "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb
")

(defparameter *example-input2*
  "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
")

(defparameter *input*
  "66: 69 116 | 9 115
91: 95 9 | 109 69
14: 110 69 | 15 9
4: 119 9 | 61 69
17: 9 23 | 69 93
37: 118 69 | 94 9
68: 9 80 | 69 19
117: 37 9 | 45 69
132: 9 109
74: 9 25 | 69 126
102: 122 9 | 6 69
98: 89 9 | 99 69
113: 83 9 | 7 69
92: 9 16 | 69 50
33: 9 39 | 69 133
134: 95 69 | 7 9
57: 9 7 | 69 110
31: 9 66 | 69 51
47: 9 3 | 69 83
21: 69 123 | 9 129
104: 9 86 | 69 7
40: 69 24 | 9 34
32: 9 44 | 69 88
45: 49 69 | 48 9
2: 69 88 | 9 23
5: 3 9 | 110 69
108: 20 86
76: 97 9 | 113 69
56: 9 121 | 69 58
29: 69 13 | 9 82
123: 86 20
46: 23 69 | 95 9
19: 7 69 | 109 9
12: 93 9 | 95 69
75: 85 9 | 128 69
127: 93 69 | 83 9
61: 9 7 | 69 23
78: 9 132 | 69 107
121: 88 69 | 23 9
60: 33 69 | 76 9
73: 26 69 | 100 9
116: 9 60 | 69 75
93: 69 9
38: 9 106 | 69 110
107: 9 7 | 69 86
82: 5 69 | 64 9
58: 44 69 | 23 9
34: 69 57 | 9 104
124: 69 106 | 9 44
109: 69 9 | 9 9
48: 69 44 | 9 83
28: 9 83 | 69 7
64: 9 93 | 69 7
54: 9 102 | 69 40
94: 83 69 | 7 9
80: 9 86 | 69 3
62: 86 69 | 83 9
42: 125 69 | 70 9
71: 69 112 | 9 114
8: 42
131: 69 44 | 9 7
88: 69 9 | 9 69
87: 15 69 | 7 9
3: 9 9 | 69 69
9: \"b\"
41: 21 69 | 78 9
65: 9 69 | 69 20
128: 134 69 | 101 9
44: 9 69 | 69 69
51: 9 18 | 69 54
55: 69 79 | 9 5
95: 69 69 | 69 9
110: 9 69
22: 92 69 | 10 9
67: 9 44 | 69 23
7: 9 20 | 69 69
90: 69 96 | 9 68
53: 111 9 | 27 69
125: 69 72 | 9 22
11: 42 31
23: 9 69 | 9 9
114: 69 105 | 9 131
81: 53 69 | 29 9
69: \"a\"
1: 86 9 | 95 69
18: 9 73 | 69 90
83: 20 20
130: 89 69 | 47 9
89: 44 9 | 93 69
15: 9 9
0: 8 11
105: 69 110
13: 135 9 | 14 69
6: 69 105 | 9 132
103: 120 69 | 71 9
85: 1 69 | 2 9
96: 89 9 | 12 69
101: 69 95 | 9 23
39: 69 15 | 9 109
133: 88 69 | 65 9
86: 69 20 | 9 9
122: 127 9 | 43 69
20: 69 | 9
52: 69 109 | 9 3
119: 15 69 | 88 9
77: 9 7 | 69 93
50: 87 9 | 61 69
129: 9 65 | 69 110
97: 69 95 | 9 93
111: 59 69 | 35 9
115: 69 84 | 9 41
84: 36 69 | 4 9
72: 117 9 | 74 69
135: 110 69
112: 62 69 | 124 9
43: 69 3 | 9 15
118: 109 9 | 93 69
49: 69 23 | 9 83
26: 69 67
63: 106 9 | 44 69
70: 103 69 | 81 9
25: 17 9 | 77 69
36: 91 9 | 32 69
10: 69 98 | 9 56
30: 9 109 | 69 3
126: 9 28 | 69 63
16: 9 38 | 69 30
99: 106 9 | 109 69
59: 23 9 | 3 69
120: 130 9 | 55 69
27: 69 91 | 9 108
24: 46 69 | 52 9
35: 9 93 | 69 109
100: 9 101 | 69 80
106: 69 69
79: 44 69 | 65 9

bbabbbaaaaaabbabbaabaaabbaababbbabbbabbbababbbbbbbbabbbbbbabaaaa
aabbbabbabaaaababbbaaabb
baababbbaababaaaabbaababbabbbaaabaaaabbbaaaabbaaabbaabba
abbbbaaabbaaaababbbabaaabbbabbbbbabaabab
aaabbbbbabbbbbabbabbaabbaaaaababababbbbaaaaabbbabbbbabbababbaaabaaaababa
bbbabaaaabbaaabbbababababababaaaabaabbbabaaaabbbabbabbabbbaaabbaaabaaaaa
bbaaaabbaabbbaabbabaaaba
abaabbabbaaaaabbbbbabaabbabbbababaaababb
baaaaabbbabbbbbbabababba
baaabbbaabbaabababbbaaab
baaaabaababbbbbababbbbaaabbabbaabbababababababbabbbabaab
baabbaaaaaaaaaababbaaabaaabbabaaabbababaaaabaaaaabbaabaaababaabbbaabbbaaaabbabbbabbbbbbb
ababbbaaabaabaababababbabbbaabbaaaabbbbbababbabaabbbbbaa
abaabbaabbbbbbbaaaabbbbaaaababba
ababaaabbaabbaaabbbaaaba
bababbbbbaabbabbabbabaaaabaababababababababaabab
bbaaaaababbbaababbbbbbaa
abaabababababbbaababaaaa
aabbababaaaababababbbbba
aabaababbbaaababbabbabab
baababababbbabbbbbbbbaba
bbbbaaaabbababaabbbabbabababbbab
baaabbbbbbabbbbbaaaabaaa
ababbbbabbaabbaaaababbaaaababaaabbbbaaabababbabbaababbbb
bbbbaababababbbbbabbaabbbabaaaba
abbbabbababbbaaaababbbbb
abbbbbabbabbabbbbaaabbabbaaaabbbbbaabaaabaabbaba
aabbabbabbbbbbabbbabbaaa
bbbbaaaabaababbbbabbbbab
bbababaaabbaaabbabaaaabb
bbbbbbbabaabababbbbababbbabbbaaaababaaaa
abbbbbaabbabbbaabbbababbababbaabbaaabbab
aaabaabbbbabababaaaabababababaabbbabbabb
abbabbabbbaaaabbabaaaababbbaabbabaaabaaa
babbbabbbbbabaaababaaabb
bbaaaaabaabbaaaabbbabbbb
abaaabbbbaababbaaabaaaab
bababaababbaaabbaaaabaab
bbaabaabbabaaaaabbbbbbbaaabaaabaaaabbbaabbaabbbabbaaaabaaabbabbbbbaaaaaabaabaaaaabbaaaba
abbabababbbbababbbaabaab
ababababaabbaabbbbabbaaa
bbbbbaabbbbabaaabaaaabba
aababaaabababbbabbababababaabbbaabbaaabababbaabbbaabbbaa
aabbaabaabbbbabbaabababb
aabaaaaaabbbbbbbabababbbbabaaaabbbaabbba
bbaaaaabaaabaabbaabaaaab
aaaaaaaaabbaaaaabbbbaaababaaabab
baababbaaaaaababbbbaaabb
abbbbaabaabbaabbbaaabbaaabbbbabbabbbaaaabaaaaaaaaabaabba
baabaabaaabbbabbbbaaaabaabaaaaabaaabaabbbabaabbb
abaaaaabbabbbabbbababbaaaabaaabbaabbababbabbbabababaabaabaabbbaa
aaaaaaabababbbbaaabaaabaababababababbabb
abbbabaabbabaababaaaabaa
abbbbbaabababbbbbabaaaba
aabbaabbbaabaaaabbaaabba
bbabababaaaaabaabbbbaabaaaaababb
aabbaabbbbbbbaabaaabaaba
bbbbbaabbabbbbbbabbbaaab
bbababbaabbbbabbbaabbbab
baaabbbbbababbaaaaabbbab
bbaabbbbbbbbbababaaaabbbbaaaababbbaababa
aabaabababbbbaabaaaaabba
abaabbbbaabbbaaabbbabbba
baabaaaaabbbaabaaabababb
baaabababbbababbaaaaaabb
babaababbaaaababbaabbaaaaaaabababbbbbbaabbaaabbbaaaabbabaaaaaabbaabbaaaaaabbbaaaaaabaaba
aaabaabbaaaababaaaaaaaabaabbbbbb
abbbababababbbbabaaaabab
abaabaabbbabababaaabbaaa
bbbbababaabaaaaaaabbbabbaababaabababaaabbbbabbab
bbbbaaabaabbbabbabbbabbabaababbbbababaaa
bbaabbbbaaabbabbaaababaaaabaaababbaabbba
aabaabbabbabbbaaaaaaabbb
aabbabaababbbabbaabbbbab
baababbbbbbbbbabbabbaaaababaabab
babbaababbaabbaaaaaaaaba
aaabbbbabbaaaabbaaabbaaa
aaaabbbbbaaababaababbbbabababbababaabbab
baaaaabbbaaabaababbaaabbabaabbaabbbbaaaababbaaaa
babbabaabbbbaaaababbbbaabbbbabbb
abbbbabaaabababababaabab
bbaaaaabababaababababbab
ababbaabaabbaaaabaabbaba
bbaabbaaabaababaaaabababbbbbaaba
babbbbaaaabbabbaaabbbbbb
baaabbaabababbaaabababaa
aaaabbbabaabababbabbaababbbbabbaaaabaabbaaaabababbbaabbb
bbbbbbabbaaabbaaabbbbaabbbabaababbbbbaaabbabaaabbabbbbba
baabaabaaabaabbaaabbababbabbabaabbabbaabbaaabaab
babaaaaaabbbbaabbbbbbaba
bbbaababaabbbabbaaababba
bababbbbbaaabbbbabbbabbaababbaababbbaaabbbbaaaba
abbbababbbbbaaabbabbaaab
bababaababaabbbabaaabbbbbaabababbbbaabaa
bbbaabbbaabbbbbaabaaabaaaaabbbbbaababbbbbbababbabbabababaaaababaaaabaabb
baaaaabbabaabaaaabbbbaaaaababababbbbabbb
bbbbaabbbbaaabbbbaabbbaabaaababbbbaababb
abaaaabaaabbababbbaabbba
bbabaabbbbaabbaaababbaabaaababaa
aababbbaabaabbbaaababbaabaaabaaaaabbbbba
babbaabbbbbbaabaaabbabbaaabbabaabbaabbab
aababaabbbbababbaabbbaababbabbabbaaaabaaabbaabaabbbaaabb
abaabbbbabaabbaababaaaba
aabbbaabbaaabababbaaabba
baabaaaaaaaabababaaaabab
bbaaaabbbaaabbbaabababba
bbaaaaabaabaaabbaaaaabbb
baaababaabbaaaaababbbbbbabbbbbaabbbbbbabababbbaa
abbbbabaababaabaaaaabaab
baaababaabbbabbbbaabbaab
abaabbaabababaababbaaaaaababaabb
abaababaaabbabbaabaaabba
bbaababbaabaaabbbababaabbbbabbab
babbaaaaabbabbbaabababbb
aabbaaabbabbbbaaaabbbbba
bababaaababaaababbbabbba
bababbbaabbaabababbbabbbbbababababbbbbaabaaababb
abbabbbababbbbaaaabbaabbaabbaaaaabbabbaa
aaaabababbabbbaaaabaabaaabbabbabbbbbaaaabbaaaababaabbabbbbbbbbbababbabaaaabbbaab
bbbaaaaabbaabbbaaabaabbaabaaababbabbbbbabbbabaab
bbaaaababbbaababbabbbaab
aabaaabaaaabaabbbbabbaaababbababbabaababbbbaabba
aaaabababaababbaaabaaabaababaababbaaabbbbabbbaab
bbaabbaaaaaaabababbabbbabaaaabbb
ababbababbabaababbbabbbb
bbbbbbabbbaabbbbbbbabbbbbbbbbbaaaabbbbabaaabaaba
aabaaababbaabbaababbaabaabbaabbaababbbbb
bbaaaaabbbbaabbabbabbabaabaaaaaababaabab
aababaaabbbbaabbbbababbabbabaaaabaaaababbaaaababbabbbaabbaaabaab
aabbaaaaabbbaaaaabbaaaaabbbabaab
aababbbabbbaababaabababaababbbaaabaaabab
bbbaaaababaaabaaaaaabbabaababbbaabababaaaaaabaabaaababab
abaaaabbabababbaababbbab
bbabaabbaababababaabbbbb
babbabaabbabbbbbaaabaabbbabbaaaabaaabbba
aaaaabaababbabaaabbabbbb
bbaaaabbaabaaabbbaabbaba
abaaababaaabbbbbbbabbababbaaabaa
bbbbaababbbabaaababbabab
ababababaababaaabababbbbaaabbaababbbbabababaabba
babbaabaabaabbbbbaabaaaaaaabaabbbaabbabb
aaabaaaaababbaabaabbabaaabaabbabbbbbbaaabaaabaaa
baaabbaabaabaabbbaabbbab
aaaabbbaabaabbbbabbbbbaabbbbbbbbababbbaa
bbabbbaabababaababbaabaa
bbabaabaabbababaabaaaaabaaaaaaaabbbaabba
bbaababbbaabbaaabaaaaaba
baabbabbabbababbbbbbbbaabbbaaaaabaaaabbb
babaaabaaababbabbabaaaaaaaabbaaaaabbabbabaaabbbabbaabbab
babbabaaabaaaabaabbbababbaaaabba
abbababaaabaabbaabbbaaaabbaabaababbbbbabaabbbaaaaaababbaabbbbbbb
abbabbbaabaabaaaaababaabaaaabbbbabbaaaaababababbbbabbaabaabbababaaaabaabaaabababbbabbaab
bbaababbaaaaabaaaabbbaababbbbbab
babbbabbaaabaaababaabbbabbbaaaba
aaabaaabbaababbaabbbbbaaaabbbaba
bbaaabababaaaabaababaabaaabbbabbbbbbaabbbbaabbaababbbbabbbabbabbbababbab
abbbabbabbbbaaababbbbaabbaaaaaab
babbbabbaabbaabaaabbaaabbbbbaaabbaababbbaaaaaabb
aabbaabaabaabbabbbbbbbbabbbaabbb
abaaaaabbbbbaababaaaabab
abbbbbbaaabaaaaabbabaaaa
bbbbbbbaaabababaaaabaaba
bababbaababaaaaababaaaba
abbbaababaababbbbbbbabbabbbaabaa
abbbbbaabbbababababbbbbbbaaaabaabbbaaaba
abaaaabaabaaabbbbaababaa
bbaabaaaaaaaababaabbaabbabbbbbbabaabbbbabbababba
baabbaabaaaaabaabaabaabbbbabbbab
bbaaabbaaabbabbaaaaaaabbbbbaabbabbaaaaaabaabbbaa
aaaaaaabbbbbbbabbbababaabbababbb
abbbabbbbabbbaaaabababbb
aaabaaabaaaabbabaaaabbbb
babbaaaabbbbbbabbabbabba
ababababaabbabaabbabbbbbbaabbbaa
aabbbaabaaaaaaabaaababbb
aaaaabaaaaabbbbaaaaabbababaabbababaabababaaaabaa
abbbbaabbbabaabbababaaabaaabaaabbaaaabbabbaaabba
aaabbbbabbbbaabaababababbababbbabaabaabbbbbaabbb
abbbababbaababbababaabaa
ababaaababbbbaabbbabbabb
babbbbbbbaababbbbbbabaaaabababbbbaabbabb
aabaaabbaabbaaaabaaaabab
abbbabbbbababbbbbbabababababaabbaabbbbba
bbbabaaaaababaabaaaabbaa
bbbbaaabbabbaababbbaabba
ababababbaaababababaaaaaaaabababbaaabbbbbbaaabaaaaababbabaaaabba
bababbbbbbaaababaababaabaabbababbbbbbbba
baababbaaabaabababbaabba
bbbaababbbbbbbbabbaabbab
babbbbaababbbbbbbaabbbba
aaabaabbabbbbaabbbaababa
abbbaaaaaaaaabaabbabbaab
aabababbababaabaabbbbbbbaabbaababbaaaaaaaaabbbbaaaaabbaa
bababbbbaaababababbabababbbbbbbaababaaaaaaabbaaa
bbbbaaababbabbababababaa
bbbbbaabaaaababbaabbbbba
baabbaaabbbbabbbbbabaabaaabaabbbaabbbbbb
bababbaababbbbbbbaaaaabbbbaaabab
aabbaaabbbbbbbabaababbabbbaaabaa
bbbbaaabbaababbaaababbab
ababbaabaabbababbaaaabaa
baabaabbabbbabbbbaaaabbb
abbabbbabbbaababbbbaabba
abaaabbaaaabbabbababaabbbaaaababbaaabbbbaaabaabb
baabaabbbaaaaabbbabbaaaaaabbabbbbbbaabbb
abbaabababaabababbaababbabbaabaa
bbaaaaaabbaaaabaaabbababababbbab
aabbaaabbababbaabbbbabbbbaababababbaabba
bbbbaabbaababaababbbbaabbbabbbbaababbbaaabbbbbbbbbababbb
aaaaababaaaababaabbbababaabababaabbaaabbbabbbbab
aaabbbbabbabaabbbbaaabaa
bbabababbabaaabbbbbaaaba
aabbabbabbbabababaaabaab
bbbbaabbabbbabbbaabbbbbabaabababaaabbbab
baaabbbbaabbbabbbbaaaaaaabbbaabbaababbbb
bbaaabababbbaabaabbbaababababbabbbbabbab
bbbaaaababbaaababbbabbabbbbbbaaaababaabb
abaababbbabbbbabaababbabbaaaaabbbaababaaabbaabaabbababaaabaababb
ababbaabaabaabbaabbabaaa
bababbaababbaababaabbaabaabaaaaabaabaabaaababbabbbabaaaa
babaaaaaaabbbabbaaabbbaa
abbbbaabbbbbaababbbaababaaaaaaababababbb
bbabababbababbbbbbbbbaaa
aabbbaabbababbaaaabaaababbabbabb
baababbbabbbbbbabbbbbbbaaaabababbaaabbab
bababbbbabaabbaaabbbbaaabaaaabbb
abbaabaaabababbbaaaaaaaaaabbaabbbaabaabb
bbabbbbbbbabbbbabbbbaabbabaaaabbbaabbaba
bbaabbaabbababaabbaaababbabaaabb
bbaaaabbbaabaababbbbabbaababaabb
aaaaaaababaabbbabbbbabaa
bbbbaabaabababbabbaabaaabbababbaababbbbbaaaabbaabbbabbbabbaababb
abaaabbbbbbbababbbaababa
abaaaababbbbabbaabababba
aababbbabbbbaabbbababbbbabaaaaababbbaabbbababbab
bbbbaaaaabaabaabbbbbababbaaaaaba
babbaaaabaabbaabaaaaabba
bbbbaaaabaababbabbbaabba
ababababbbaaaaaaabbabaab
bababbbabbbaababaabbabaaabbabbbabbabbbab
baabaaabbabbbbbbbbbbbaabaabababa
ababbbbaabaaaabababbbbba
aaaaababbbabbbaababaaaba
baabababbaaaaabbbbababbb
bbbbabbabaabaaaababbbbba
aababaaabbabbbabbbbbbabbbbbaabbbbaabbabb
bbabaabaabbabababbbabaaaabaabaaababaaaba
aabbbbaaabbbbabbbabbabab
abaababaababbbbbbababbab
bbbabaaaabbbbabaaaabbabb
aabbaabbabbabbabbabbabba
bbababababaabbbbbabaaaab
bbbbaaaaaabaaabbbaabbbbb
aaaababbbaababababbabbaa
abaabbaaaabaaabababbbbba
aababbaabbaaababbbbbbbbababbaabb
babbbababbbbbaabbaabbbabbaaaababbbaabbabbbbaaabbaabbbabbbaababaa
bbaaaaabbaabaabaabbaaabbababaababaabaabbabbabbaabbaaabbababbbbab
abbabbbabaabaabbabbbaaab
babbababbaabbabaabaaabbabbababbbaaababbb
baababbbbbababaaababaaaabbabbaaa
bbbabaaaaaabbbaaaaaabbabbabababbabbbbaaaaaababab
ababaaabbaaabbbbababaaabbbbbaaaababbaabaabbbbabbbbabbaaa
abaaaaababbaaaaabbabbbaabaaaabaababaaaba
abaabababaabbaabbbbaaaab
babbbbbbbaaabbaaabbbbababbaaabbbabababaabbbaaaba
baabaaabaaaaaaabbbabaabbbbabbaba
aababbaaabbbaaaaabababba
abbbbabaabaabbaaaaaaaaababbbaaaabbabbaaababaaaab
bbaaaabbaababaabaabaaabaabbabbbb
bababaabbabbbaaabbaabaaa
bbbaabababbbbaabbabbabaabbaabaaa
babaaaaaabaaabbbbbababbb
abbbbbaabaabaabababbabba
baababbaaabbababababbbaa
aaabbbbaaaabababbabaabba
abaabbbabbbaabababaaabab
babbbaaaaabaaabababbbaba
abaabaabbabbaabaabaababb
aaaaaaabbabbaabaaaabbbab
bbabbbaabaabaababbaaaaab
bbaaaabbbbbbbbabababaabb
bbaababbbaaabbbabbaabbab
aaababababbabbabbabbabab
aabbaaaababaaaaaabbbabbaabaabbbaababaaaa
aabaaaaaabbaabababaaabab
baabaaabbbbababaababbbbb
ababababbbbbaababbaabaaa
bbbababaaababbbaabaaabab
bbbababbbbababababababbb
baababbbbbbbbbabaabbbbaabbbbbaaa
ababbbbaaabbaabbbbaabbbb
baababbababababbbbbbbbbaabbbbaabbbaaaaabbbaabbab
abaabbbabaababbaabbaabaa
aabbabbabbbbaaaaabaababb
baababbaabbaababaabaaaaabbbabbba
aabaaaaabbbbababbbbbaaaabbbabaabaababbab
aaaaaaaaabbaaabababbbbab
baaabbbbabbbaaaaababbbab
abaababaaababaaabababbaaabbbababbbabbbaaaaababbaaaaaabbaaaabbbbb
aabbbaaaabaabaaabbbbbbababbbbaaabbbabbaa
babbaaaaaabbbaaaabbbbbaaaabbabbabbbaabaa
bbbababbbabbbbaaaabaabaa
babbbabbabbbbbaaabbabbbb
aababaaabbaaaaababaaaaaabbbbbbababababbbbaaaabbabaababaababbabbaabbbbaaababababa
bbbabaaabaababbaaabaaaab
baabaababbabbbbbbaabbbbb
bbbbbbabaaababbbabbabbaabaabbbba
bababbabbaabbaaabaababbbabbbaababbabbaaaaababbabbbbbaaabbabbaaaaabaabbba
abbaaabbabbababaabaaabaa
bbaaababaabbabaaabaaabaa
bbabbbaaaaaaaaaaaaaabaab
bbaaaaabbbababbabbbaaabb
aaaaaaabababbabababaaaab
abbabbabbabbbbaabbabbabb
aabaaaaaabbbaaaaaabbabababaaabbbbbabaaabbabaabaabaaaabaa
bbbbbaabbbaaaaaababbbbbbaaaabbababbaabbbabbbbbabbaaaabab
aaabaaaaabbbabaabbaaaaba
aababaabaabbaaaabaaaaaaa
aaaabbbaaaabababaabbbaababbabaaabaaaaaaa
baabbaabbaabaabbbababbbbbbbabbaa
bbbaababbbbbaaaaaaaabaab
bababaabbabbabaabbabbaaa
baabbaaababaaaaabbaaaaaabbbabbbaaabbbaaabbbabaaaaababbbbbbbaaaab
abbaababbaababababaabbaaababbaababbaabbbaaaaaabb
babbabaaaaababbaabbaabaabbbbabba
babbaabbbabbaabbaaaabaab
aababbaaaaabaaabaabbbaabaabbabbaabbbbbbabbaabbbbbababaaa
aaaabbbababaaaaaaabaabbb
bbaababbabbbaaaaababaaabbbabbbbaaabbbbaababbababbbabbabb
aababbbaaaabaaaaabaababb
baaababbabbbbbbbbabaaabbabaabaabbbabbaba
abbabbabaaaaaabbaaabbabbbbbabaababbababbbbbaabba
abbbbabaaabbababbbbaaaba
abbbaababbabbabbbaabbabbbbabaaaa
abbbbaabaabbbabbaaaabbaa
aaabbbbaaaaabbababbaaabbaaaaabbb
bbbbaaabaaabaaabbaabbbaa
babbbabbbabbabaaabbabbbb
ababbbbaabbbabbbaaabbaaa
bbabbbbbababbaabaaaabbbb
bbbababbbbbbbbbabbbabbbb
abbbabbbbababbbbabbabbbb
")

(defun peek () (peek-char nil *standard-input* nil nil))
(defun consume () (read-char))
(defun eof () (null (peek)))
(defun looking-at (ch) (eql ch (peek)))
(defun must (ch)
  (assert (char= (consume) ch)))

(defun read-while (test)
  (with-output-to-string (out)
    (loop for c = (peek)
          while (and c (funcall test c))
          do (write-char (consume) out))))

(defun parse-number ()
  (parse-integer (read-while #'digit-char-p)))

(defun consume-whitespace ()
  (loop while (looking-at #\Space)
        do (consume)))

(defun parse-document ()
  (prog1 (list (parse-rules)
               (when (looking-at #\Newline)
                 (consume)
                 (parse-messages)))
    (assert (eof))))

(defun parse-rules ()
  (loop while (digit-char-p (peek))
        collect (parse-rule)))

(defconstant +rule-zero+ 0)

(defun parse-rule-number ()
  (+ +rule-zero+ (parse-number)))

(defun parse-rule ()
  (let ((rule-number (parse-rule-number)))
    (consume-whitespace)
    (must #\:)
    (let ((disjunction (loop collect (progn
                                       (consume-whitespace)
                                       (nconc (parse-sub-rule)
                                              (when (= rule-number +rule-zero+)
                                                '(eof))))
                             while (progn
                                     (consume-whitespace)
                                     (when (looking-at #\|)
                                       (consume)
                                       t)))))
      (must #\Newline)
      (list rule-number disjunction))))

(defun parse-sub-rule ()
  (loop for parsed = (progn
                       (consume-whitespace)
                       (cond
                         ((digit-char-p (peek))
                          (parse-rule-number))
                         ((char= #\" (peek))
                          (must #\")
                          (prog1 (consume)
                            (must #\")))))
        while parsed
        collect parsed))

(defun parse-messages ()
  (loop while (and (not (eof))
                   (alpha-char-p (peek)))
        collect (read-line *standard-input*)))

(defun parse-document-string (input)
  (with-input-from-string (*standard-input* input)
    (parse-document)))


(defun rule-disjunction (rule rules)
  (second (assoc rule rules :test #'eql)))

(defparameter *rule-depth* 0)

(deftype rule-number () `(integer ,+rule-zero+ *))
(deftype parse-index () '(or nil (integer 0 80)))

(defun without-first (seq)
  (make-array (- (length seq) 1)
              :element-type (array-element-type seq)
              :displaced-to seq
              :displaced-index-offset 1))

(defun eval-charmatch (strings char)
  (loop for str in strings
        when (and (not (zerop (length str)))
                  (char= char (aref str 0)))
          collect (without-first str)))

(defun eval-eof (strings)
  (remove-if-not #'(lambda (str) (zerop (length str))) strings))

(defun eval-subrule (messages sub-rule rules)
  (declare (type list messages)
           (type list sub-rule)
           (type list rules))
  (loop for term in sub-rule
        while messages
        do (setf messages
                 (cond
                   ((typep term 'rule)
                    (eval-rule messages term rules))
                   ((characterp term)
                    (eval-charmatch messages term))
                   ((eq term 'eof)
                    (eval-eof messages))
                   (t
                    (error "Bad term in rule: ~S" term))))
        finally (return messages)))

(defun debug-print-depth ()
  (fresh-line)
  (dotimes (i (* 4 *rule-depth*))
    (format t " ")))

(defun flatten (lst &aux result)
  (labels ((rflatten (lst1)
             (dolist (el lst1 result)
               (unless (null el)
                 (if (listp el)
                     (rflatten el)
                     (push el result))))))
    (nreverse (rflatten lst))))

(defun eval-rule (messages rule rules)
  (declare (type list messages)
           (type rule-number rule)
           (type list rules))
  (let ((disjunction (rule-disjunction rule rules)))
    (loop for sub-rule in disjunction
          collect (eval-subrule messages
                                sub-rule
                                rules)
            into remaining-messages
          finally (progn
                    (setf remaining-messages
                          (remove-duplicates (flatten remaining-messages)
                                             :test #'string=))
                    (return remaining-messages)))))

(defun valid-message-p (message rules)
  (eval-rule (list message) +rule-zero+ rules))

(defun count-valid-messages (document)
  (destructuring-bind (rules messages) document
    (count-if #'(lambda (message)
                  (valid-message-p message rules))
              messages)))

(defun part-one (input)
  (count-valid-messages (parse-document-string input)))

(defun update-rule (rule)
  (let ((rule-number (first rule)))
    (cond
      ((= rule-number 8)
       (with-input-from-string (*standard-input* "8: 42 | 42 8
")
         (parse-rule)))
      ((= rule-number 11)
       (with-input-from-string (*standard-input* "11: 42 31 | 42 11 31
")
         (parse-rule)))
      (t rule))))

(defun update-rules (rules)
  (mapcar #'update-rule rules))

(defun update-document (document)
  (destructuring-bind (rules messages) document
    (list (update-rules rules) messages)))

(defun part-two (input)
  (count-valid-messages (update-document (parse-document-string input))))

(defun test ()
  (assert (= 2 (part-one *example-input*)))

  ;; This is the answer to Part One.
  (assert (= 165 (part-one *input*)))

  (assert (= 3 (part-one *example-input2*)))

  (assert (= 12 (part-two *example-input2*)))

  ;; This is the answer to Part Two.
  (assert (= 274 (part-two *input*))))
