(defpackage semistatic
  (:use :cl)
  (:export
   :start-blog
   :stop-blog))

(in-package :semistatic)

;;; POST

(defclass post ()
  ((path :type pathname :initarg :path :reader post-path)
   (doc :type plump-dom:root :initarg :doc :reader post-doc)
   (universal-time :type integer :initarg :universal-time :reader post-universal-time)
   (title :type string :initarg :title :reader post-title)
   (blog :type blog :initarg :blog :reader post-blog)))

(defun make-post (path &optional (blog nil))
  (let* ((doc (lquery:load-page path))
         (time (cl-date-time-parser:parse-date-time
                (lquery:$1 (inline doc) "time" (text))))
         (title (lquery:$1 (inline doc) "title" (text))))
    (make-instance 'post
                   :path path
                   :doc doc
                   :universal-time time
                   :title title
                   :blog blog)))

(defmethod post-tags ((post post))
  (coerce (lquery:$ (inline (post-doc post)) "a[rel=tag]" (text)) 'list))

(defmethod post-article ((post post))
  (lquery:$1 (inline (post-doc post)) "article"))

(defmethod post-images ((post post))
  (coerce (lquery:$ (inline (post-doc post)) "img") 'list))

(defmethod post-slug ((post post))
  (cl-slug:slugify (post-title post)))

(defmethod post-time ((post post))
  (lquery:$1 (inline (post-doc post)) "header" "time"))

(defmethod post-link ((post post))
  (let ((directories (pathname-directory (post-path post))))
    (format nil "/~{~A~^/~}"
            (subseq directories (- (length directories) 3)))))

;;; TRANSFORMERS

;;;;;; Resize images

(defun create-thumbnail (filename thumbname width height)
  "Create a thumbnail the image in FILENAME with a max size of WIDTH x HEIGHT
    pixel (but with the original aspect ratio) and save it in THUMBNAME."
  (magick:with-magick-wand (wand :load filename)
    (let ((a (/ (magick:get-image-width wand)
                (magick:get-image-height wand))))
      (if (> a (/ width height))
          (magick:scale-image wand width (truncate (/ width a)))
          (magick:scale-image wand (truncate (* a height)) height)))
    (magick:write-image wand thumbname)))

(defun resize-image (post image output-directory relative-path)
  "Resize the source of the IMAGE tag, saving the scaled image to
    OUTPUT-DIRECTORY, and replacing the source using RELATIVE-PATH."
  (let* ((old-src (lquery:$1 (inline image) (attr 'src)))
         (new-src (namestring (merge-pathnames old-src relative-path)))
         (old-path (namestring (merge-pathnames old-src (post-path post))))
         (new-path (namestring (merge-pathnames old-src output-directory))))
    (create-thumbnail old-path new-path 800 600)
    (plump:set-attribute image "src" new-src)))

(defmethod xform-resize-images ((post post))
  (let ((blog (post-blog post)))
    (if blog
        (let ((path (merge-pathnames "static/" (blog-path blog)))
              (prefix (blog-static-prefix blog)))
          (mapcar (lambda (image) (resize-image post image path prefix))
                  (post-images post)))
        (warn "Can't xform-resize-images, post not linked to a blog.")))
  post)

;;; Time ago

(defun humanize-time-period (time now)
  (let ((possible-outputs (list))
        (seconds (- now time)))
    (flet ((humanize (label divider)
             (push (cons (truncate seconds divider) label) possible-outputs)))
      (humanize "minute" 60)
      (humanize "hour" 3600)
      (humanize "day" 86400)
      (humanize "month" (* 30 86400))
      (humanize "year" (* 365 86400)))
    (let* ((humanized (car (remove-if (lambda (item) (zerop (car item)))
                                      possible-outputs))))
      (if humanized
          (let* ((period (car humanized))
                 (plural (if (= 1 period) "" "s"))
                 (label (cdr humanized)))
            (format nil "~a ~a~a ago" period label plural))
          "just now"))))

(defmethod xform-humanize-published-date ((post post))
  (let ((plump:*tag-dispatchers* plump:*xml-tags*))
    (plump:clear (post-time post))
    (plump:set-attribute (post-time post) "datetime"
                         (local-time:to-rfc3339-timestring
                          (local-time:universal-to-timestamp
                           (post-universal-time post))))
    (plump:parse
     (humanize-time-period (post-universal-time post) (get-universal-time))
     :root (post-time post))
    post))

;;; Permalink

(defmethod xform-header-permalink ((post post))
  (let ((plump:*tag-dispatchers* plump:*xml-tags*)
        (title (lquery:$1 (inline (post-doc post)) "header" "h1")))
    (plump:clear title)
    (plump:parse
     (spinneret:with-html-string
       (:a :href (post-link post) (post-title post)))
     :root title))
  post)

;;; BLOG

(defclass blog ()
  ((posts :type list :initform (list) :accessor blog-posts)
   (transformers :type list :initarg :transformers :reader blog-transformers)
   (path :type pathname :initarg :path :reader blog-path)
   (static-prefix :initarg :static-prefix :reader blog-static-prefix)))

(defun find-post-paths (posts-path)
  (directory
   (make-pathname :defaults (concatenate
                             'string (namestring posts-path) "/**/index.html"))))

(defun make-blog (path &key (static-prefix #p"/static/") (transformers (list)))
  (let ((post-paths (find-post-paths
                     (merge-pathnames "posts" path)))
        (blog (make-instance 'blog
                             :path path
                             :static-prefix static-prefix
                             :transformers transformers)))
    (dolist (post-path post-paths)
      (blog-add-post blog (make-post post-path blog)))
    blog))

(defmethod blog-add-post ((blog blog) (post post))
  (dolist (transformer (blog-transformers blog))
    (funcall transformer post))
  (push post (blog-posts blog)))

(defun posts-by-recency (posts)
  (let ((posts-copy (copy-seq posts)))
    (sort posts-copy #'> :key #'post-universal-time)))

(defmethod blog-posts-by-recency ((blog blog))
  (posts-by-recency (blog-posts blog)))

(defmethod blog-toc-html ((blog blog))
  (let ((spinneret:*html-style* :tree))
    (spinneret:with-html-string
      (:ul
       (dolist (post (blog-posts-by-recency blog))
         (:li (:a :href (post-link post) (post-title post))))))))

(defmethod blog-tag-posts ((blog blog))
  (let ((tag-posts (make-hash-table :test #'equalp :size 255)))
    (dolist (post (blog-posts blog))
      (dolist (tag (post-tags post))
        (unless (gethash tag tag-posts)
          (setf (gethash tag tag-posts) (list)))
        (push post (gethash tag tag-posts))))
    tag-posts))

(defmethod blog-tag-list-html ((blog blog))
  (let ((tag-posts (blog-tag-posts blog)))
    (spinneret:with-html-string
      (:ul :class "tags"
       (alexandria:maphash-keys
        (lambda (tag)
          (:li (:a :href (format nil "/tag/~a" tag) tag)))
        tag-posts)))))

(defmethod blog-find-posts ((blog blog) year month slug)
  (let ((link (format nil "/~a/~a/~a" year month slug)))
    (remove-if-not (lambda (post) (equalp (post-link post) link))
                   (blog-posts blog))))

;;; PAGES

;;; TODO: Create the PAGE abstraction, bind templates to list of posts and argumments.

(defmethod blog-render-posts ((blog blog) posts &key (header-path ""))
  (let* ((main-template (lquery:load-page
                         (merge-pathnames
                          "templates/main.html" (blog-path blog))))
         (main-element (lquery:$1 (inline main-template) "main"))
         (nav-element (lquery:$1 (inline main-template) "nav"))
         (header-path-element (lquery:$1 (inline main-template) "#header-path")))
    (plump:parse header-path :root header-path-element)
    (plump:parse (blog-toc-html blog) :root nav-element)
    (plump:parse (blog-tag-list-html blog) :root nav-element)
    (mapcar (lambda (post)
              (let ((article (post-article post)))
                (plump:set-attribute article "id" (post-slug post))
                (plump:append-child main-element (post-article post))))
            posts)
    (plump:serialize main-template nil)))

(defmethod blog-render-main ((blog blog))
  (blog-render-posts blog (blog-posts-by-recency blog)))

(defmethod blog-render-tag ((blog blog) tag)
  (let ((posts (gethash tag (blog-tag-posts blog))))
    (blog-render-posts blog (posts-by-recency posts)
                       :header-path (format nil "/tag/~a" tag))))

(defmethod blog-render-post ((blog blog) year month slug)
  (blog-render-posts blog (blog-find-posts blog year month slug)
                     :header-path (format nil "/~a/~a/~a" year month slug)))

;;; SERVER

(defvar *acceptor* nil)
(defvar *admin-acceptor* nil)
(defvar *blog* nil)
(defvar *path* nil)

;;; Blog

(easy-routes:defroute main ("/" :name "blog") ()
  (setf (hunchentoot:content-type*) "text/html")
  (blog-render-main *blog*))

(easy-routes:defroute tag ("/tag/:tag" :name "blog") ()
  (setf (hunchentoot:content-type*) "text/html")
  (blog-render-tag *blog* tag))

(easy-routes:defroute post ("/:year/:month/:slug" :name "blog") ()
  (setf (hunchentoot:content-type*) "text/html")
  (blog-render-post *blog* year month slug))

;;; Admin

(easy-routes:defroute admin-reload ("/admin/reload" :name "admin") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (reload-blog)
  "OK")

(defun start-blog (&optional (path *default-pathname-defaults*))
  (setf *path* path)
  (unless *acceptor*
    (reload-blog)
    (setf *acceptor* (make-instance 'easy-routes:easy-routes-acceptor
                                    :address "127.0.0.1" :port 4242 :name "blog"))
    (setf *admin-acceptor* (make-instance 'easy-routes:easy-routes-acceptor
                                          :address "127.0.0.1" :port 4343 :name "admin"))
    (push (hunchentoot:create-folder-dispatcher-and-handler
           "/static/" (merge-pathnames "static/" path))
          hunchentoot:*dispatch-table*)
    (hunchentoot:start *acceptor*)
    (hunchentoot:start *admin-acceptor*)))

(defun reload-blog ()
  (setf *blog*
        (make-blog *path*
                   :transformers (list #'xform-resize-images
                                       #'xform-humanize-published-date
                                       #'xform-header-permalink))))
(defun stop-blog ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  (when *admin-acceptor*
    (hunchentoot:stop *admin-acceptor*)
    (setf *admin-acceptor* nil)))
