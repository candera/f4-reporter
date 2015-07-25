(ns org.craigandera.debrief-parser.main)

;; function readSingleFile(e) {
;;   var file = e.target.files[0];
;;   if (!file) {
;;     return;
;;   }
;;   var reader = new FileReader();
;;   reader.onload = function(e) {
;;     var contents = e.target.result;
;;     displayContents(contents);
;;   };
;;   reader.readAsText(file);
;; }

;; function displayContents(contents) {
;;   var element = document.getElementById('file-content');
;;   element.innerHTML = contents;
;; }

;; document.getElementById('file-input')
;;   .addEventListener('change', readSingleFile, false);

(defn display-contents
  [contents]
  (set! (.-innerHTML (.getElementById js/document "output"))
        contents))

#_(defn parse-file
  [e]
  (let [file (-> e .-target .-files (.item 0))]
    (if-not file
      (.debug js/console "No file found")
      (do
        (.debug js/console "Found file")
        (let [reader (js/FileReader.)]
          (set! (.-onload reader)
                (fn [e]
                  (display-contents (->> e
                                      .-target
                                      .-result
                                      (insta/parse parser)
                                      pr-str))))
          (.readAsText reader file))))))

(defn main []
  (.debug js/console "In main")
  (-> js/document
    (.getElementById "file-input")
    (.addEventListener "change"
                       ;;(fn [e] (.debug js/console "file changed"))
                       parse-file
                       false)))

(set! (.-onload js/window) (fn [e] (main)))
