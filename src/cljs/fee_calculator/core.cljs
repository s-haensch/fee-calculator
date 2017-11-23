(ns fee-calculator.core
  (:require [reagent.core :as reagent]
            [re-frame.core :as rf]
            [clojure.string :as str]))


;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db              ;; sets up initial application state
  :initialize                 ;; usage:  (dispatch [:initialize])
  (fn [_ _]                   ;; the two parameters are not important here, so use _
    { :hourly-fee 75
      :hours 8}))             ;; the application state will initially be a map with two keys


(rf/reg-event-db
  :hours-change
  (fn [db [_ new-hours]]            ;; db event handlers given 2 parameters:  current application state and event (a vector)
    (assoc db :hours new-hours)))   ;; compute and return the new application state

(rf/reg-event-db
  :price-change
  (fn [db [_ new-price]]
    (assoc db :hourly-fee new-price)))


;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
  :hours
  (fn [db _]      ;; db is current app state. 2nd unused param is query vector
    (:hours db)))

(rf/reg-sub
  :hourly-fee
  (fn [db _]
    (:hourly-fee db)))

;; -- Domino 5 - View Functions ----------------------------------------------

(defn arrow []
  [:svg.arrow {:width 10
         :height 5
         :viewBox "0 0 10 5"
         :version "1.1" }
    [:path {:d "M 5 0L 10 5L -2.4968e-08 5L 5 0Z"
            :fill "#D0E7E8"}]])

(defn number-input [props]
  [:div.input
    [:div.number-input
      [:input {:type "number"
               :name (:name props)
               :value (:value props)
               :on-change (:dispatch props) }]
      [:div.stepper.input-up {
        :on-click
          #(rf/dispatch [:hours-change (inc (int (:value props)))])}
        [arrow]]
      [:div.stepper.input-down {
        :on-click
          #(rf/dispatch [:hours-change (dec (int (:value props)))])}
        [arrow]]]
    [:label {:for (:name props)} (:label props)]])

(defn text-input [props]
  [:div.input
    [:div.number-input
      [:input {:type "text"
               :name (:name props)
               :value (:value props)
               :on-change (:dispatch props) }]
    [:label {:for (:name props)} (:label props)]]])

(defn ui []
  (let [ hourly-fee   @(rf/subscribe [:hourly-fee])
         hours        @(rf/subscribe [:hours])
         net-price    (* hourly-fee hours) ]

  [:div
    [:h4.caption "Your workload"]
    [:div.input-group
      [:div.input-line
        [number-input {
          :value hours
          :name "hours-input"
          :dispatch #(rf/dispatch [:hours-change (-> % .-target .-value)])
          :label "hours" }
          ]]
        
      [:div.input-line
        [text-input {
          :value hourly-fee
          :name "hourly-price-input"
          :dispatch #(rf/dispatch [:price-change (-> % .-target .-value)])
          :label "EUR / h"
          }]]]

    [:h4.caption "Your fee"]
    [:p.output.net-price (.toFixed net-price 2)
                         " EUR (net)"]
    [:p.output.vat-price (.toFixed
                            (+ net-price (* net-price 0.19)) 2)
                         " EUR (incl. VAT)"]]))


;; -- Entry Point -------------------------------------------------------------
(defn mount-root []
  (rf/clear-subscription-cache!)
  (reagent/render [ui]              ;; mount the application's ui into '<div id="app" />'
    (js/document.getElementById "app")))

(defn ^:export run
  []
  (rf/dispatch-sync [:initialize])     ;; puts a value into application state
  (mount-root))
