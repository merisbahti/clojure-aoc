(ns aoc2020.day4
  (:require
   [clojure.test :refer [deftest is]]))

(def test-input-raw "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")
\
 (defn parse-input [input] (.split input "\n\n"))
(deftest parse-input-test (is (= 4 (count (parse-input test-input-raw)))))
(comment (second nil))

(defn get-field [field input]
  (let [matches (re-find (re-pattern (format "%s:([^\\s]+)" field)) input)]
    (second matches)))
(deftest can-get-field (is (= "def" (get-field "abc" "abc:def"))))
(deftest can-get-field-from-big (is (= "def" (get-field "abc" "fff:fff abc:def ghi:jkl"))))
(deftest can-get-field-from-whitespace (is (= "def"
                                              (get-field "abc"
                                                         "fff:fff
abc:def
ghi:jkl"))))
(deftest can-get-field-nil (is (= nil (get-field "abc" "abcq:def"))))

(def required-fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])
(defn get-fields [passport] (map #(get-field % passport) required-fields))
(defn has-all-fields [passport] (= '() (filter nil? (get-fields passport))))


(deftest get-fields-test (is (=
                              ["1937" "2017" "2020" "183cm" "#fffffd" "gry" "860033327"]
                              (get-fields "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm"))))
(deftest is-valid-test1 (is (true?
                             (has-all-fields "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm"))))
(deftest is-valid-test2 (is (false?                             (has-all-fields "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929"))))
(deftest fields-test-3 (is (= ["1931" "2013" "2024" "179cm" "#ae17e1" "brn" "760753108"]
                              (get-fields "hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm"))))
(deftest is-valid-test3 (is (true?  (has-all-fields
                                     "hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm"))))
(deftest is-valid-test4 (is (false?  (has-all-fields "hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"))))
(defn sol1 [parsed-input] (count (filter true? (map has-all-fields parsed-input))))
(deftest sol-1-testinput-test (is (= 2 (sol1 (parse-input test-input-raw)))))
(deftest sol-1-realinput (is (= 250 (sol1 (parse-input (slurp "input/day4/input.txt"))))))


(defn validate-field [field value]
  (let [result (case field
                 "byr" (let [year (Integer/parseInt value)] (and (>= year 1920) (<= year 2002)))
                 "iyr" (let [year (Integer/parseInt value)] (and (>= year 2010) (<= year 2020)))
                 "eyr" (let [year (Integer/parseInt value)] (and (>= year 2020) (<= year 2030)))
                 "hgt" (let [inches (second (re-matches #"(\d\d)in" value))
                             cm (second (re-matches #"(\d\d\d)cm" value))]
                         (cond
                           (not (nil? inches))
                           (let [inches (Integer/parseInt inches)]
                             (and (>= inches 59) (<= inches 76)))
                           (not (nil? cm))
                           (let [cm (Integer/parseInt cm)]
                             (and (>= cm 150) (<= cm 193)))
                           :else false))
                 "hcl" (->> value
                            (re-matches #"#[0-9a-fA-F]{6}")
                            nil?
                            not)
                 "ecl" (->> value
                            (re-matches #"amb|blu|brn|gry|grn|hzl|oth")
                            nil?
                            not)
                 "pid" (->> value
                            (re-matches #"\d{9}")
                            nil?
                            not)
                 throw "Error")]
    (println field result)
    result))

(defn validate-passport [passport] (map #(validate-field % (get-field % passport)) required-fields))
(defn sol2 [parsed-input]  (->> parsed-input
                                (map #(validate-passport %))
                                (filter true?)
                                count))
(deftest sol2-test-invalid (is (= 0 (sol2 (parse-input "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")))))

(deftest valid-fields (is
                       (->> [["byr" "2002"]
                             ["hgt" "60in"]
                             ["hgt" "190cm"]
                             ["hcl" "#123abc"]
                             ["ecl" "brn"]
                             ["pid" "000000001"]]
                            (map #(validate-field (first %) (second %)))
                            (every? true?))))
(deftest invalid-fields (is
                         (->> [["byr" "2003"]
                               ["hgt" "190in"]
                               ["hgt" "190"]
                               ["hcl" "#123abz"]
                               ["hcl" "123abc"]
                               ["ecl" "wat"]
                               ["pid" "0123456789"]]
                              (map #(validate-field (first %) (second %))))))

(deftest sol2-test-valid (is (= 4 (sol2 (parse-input "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")))))
