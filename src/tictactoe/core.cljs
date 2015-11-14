(ns ^:figwheel-always tictactoe.core
    (:require
              [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(def board-size 3)
(def win-lenght 3)

(defn new-board [n]
  (vec (repeat n (vec (repeat n "B")))))

(defonce app-state (atom {:text "Welcome to tic tac toe"
                          :board (new-board board-size)
                          :game-status :in-progress}))

(defn full? [board]
  (every? #{"P" "C"} (apply concat board)))

(defn straight [owner board [x y] [dx dy] n]
  (every? true?
          (for [i (range n)]
            (= (get-in board [(+ (* dx i) x)
                              (+ (* dy i) y)])
               owner))))

(defn win? [owner board n]
  (some true?
      (for [i (range board-size)
            j (range board-size)
            dir [[1 0][0 1][1 1][1 -1]]]
          (straight owner board [i j] dir n))))

(defn has-win? [board owner remaining-spots]
  (first
   (filter (fn [x]
             (win? owner
                   (assoc-in board x owner)
                   win-lenght))
           remaining-spots)))

(defn next-move [board remaining-spots]
  (let [win (has-win? board "C" remaining-spots)
        pwin (has-win? board "P" remaining-spots)]
      (cond
        win win
        pwin pwin
        :else (rand-nth remaining-spots))))

(defn computer-move [board]
  (let [remaining-spots (for [i (range board-size)
                              j (range board-size)
                              :when (= (get-in board [j i]) "B")]
                          [j i])
        move (when (seq remaining-spots)
               (next-move board remaining-spots))]
    (if move
      (assoc-in board move "C")
      board)))

(defn game-status [board]
  (cond
    (win? "P" board win-lenght) :player-victory
    (win? "C" board win-lenght) :computer-victory
    (full? board) :draw
    :else :in-progress))

(defn update-status [state]
  (assoc state :game-status (game-status (:board state))))

(defn check-game-status [state]
  (-> state
      (update-in [:board] computer-move)
      (update-status)))


(defn blank [i j]
  [:rect {:width 0.9
          :height 0.9
          :fill "grey"
          :x i
          :y j
          :on-click (fn rect-click []
                      (when (= (:game-status @app-state) :in-progress)
                        (swap! app-state assoc-in [:board j i] "P")
                        (if (win? "P" (:board @app-state) win-lenght)
                          (swap! app-state assoc :game-status :player-victory)
                          (swap! app-state check-game-status))))}])

(defn circle [i j]
  [:circle
   {:r 0.3
    :stroke "green"
    :stroke-width 0.1
    :fill "none"
    :cx (+ 0.5 i)
    :cy (+ 0.5 j)}])

(defn cross [i j]
  [:g {:stroke "darkred"
        :stroke-width 0.2
        :stroke-linecap "round"
        :transform (str "translate(" (+ 0.2 i) "," (+ 0.2 j) ") "
                        "scale(0.6)")}
    [:line {:x1 0 :y1 0 :x2 1 :y2 1}]
    [:line {:x1 1 :y1 0 :x2 0 :y2 1}]])


(defn tictactoe []
  [:center
    [:hi (:text @app-state)]
    [:h2
     (case (:game-status @app-state)
       :player-victory "You won!"
       :computer-victory "Computer wins."
       :draw "Draw."
       "")]
    [:p
     [:button
      {:on-click
        (fn new-game-click [e]
            (swap! app-state assoc
                   :board (new-board board-size)
                   :game-status :in-progress))}
      "New Game"]]
    (into
      [:svg
        {:view-box (str "0 0 " board-size " " board-size)
          :width 500
          :height 500}]
      (for [i (range (count (:board @app-state)))
            j (range (count (:board @app-state)))]
        (case (get-in @app-state [:board j i])
          "B" [blank i j]
          "P" [circle i j]
          "C" [cross i j])))])

(reagent/render-component [tictactoe]
                          (. js/document (getElementById "app")))

(defn on-js-reload [])
