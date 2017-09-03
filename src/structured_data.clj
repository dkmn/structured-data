(ns structured-data)

(defn do-a-thing [x]
  (let [x-doubled (+ x x)]
    (Math/pow x-doubled x-doubled)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))


(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn sum-pairs [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn width [[[x1 y1] [x2 y2]]]
  (Math/abs (- x1 x2)))


(defn height [[[x1 y1] [x2 y2]]]
  (Math/abs (- y1 y2)))


(defn square? [rectangle]
  (== (height rectangle)
      (width rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [p1 p2] point]
    (and (<= x1 p1 x2)
         (<= y1 p2 y2))))



(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))


(defn title-length [book]
  (let [{t :title
         a :authors} book]
    (count t)))


(defn author-count [{t :title
                     a :authors}]
  (count a))


(defn multiple-authors? [book]
  (if (> (author-count book) 1) true
                                false))

(defn add-author [book new-author]
  (let [{a :authors} book]
    (assoc book :authors (conj a new-author))))



(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))


(defn second-elements [collection]
  (let [secondify (fn [v] (get v 1))]
    (map secondify collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))


(defn stars [n]
  (apply str (repeat n "*")))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn contains-duplicates? [a-seq]
  (not= (count (set a-seq))
        (count a-seq)))


(defn old-book->new-book [book]
  (let [{t :title
         a :authors} book]
    (assoc book :authors (set a))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn author-names [book]
  (apply clojure.set/union (:name (:authors book))))

; COMMENTING OUT PRIOR VERSION
; (defn all-author-names [books]
; (set (apply concat (map author-names books))))

(defn all-author-names [books]
  (set (clojure.set/union (map :name
                               (authors books)))))

(defn author->string [author]
  (let [ name (:name author)
         dyr (:death-year author)
         byr (:birth-year author)
        year-string (cond
                      (contains? author :death-year)  (str " (" byr " - " dyr ")")
                      (contains? author :birth-year)  (str " (" byr " - )")
                      :else "")
        ]
    (str name year-string)
        ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (authors (set [book])))))

(defn books->string [books]
  (cond
    (== (count books) 0)    "No books."
    (== (count books) 1)    (str "1 book. " (book->string (get books 0)) ".")
    :else                    (apply str (count books)
                                      " books. "
                                      (interpose ". " (map book->string books))
                                  )
    )
  )

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
