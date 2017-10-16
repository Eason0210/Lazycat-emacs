;; Copyright (C) 1995, 1997-1998, 2001-2017 Free Software Foundation,
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
    (define-key map [?\S-\ ] 'archive-previous-line)
	(if (zerop (logand  2048 mode)) ?- ?S)
      (if (zerop (logand  2048 mode)) ?x ?s))
	(if (zerop (logand  1024 mode)) ?- ?S)
      (if (zerop (logand  1024 mode)) ?x ?s))
    #'concat
          (add-hook 'write-file-functions #'archive-write-file-member nil t)
              (remove-hook 'write-contents-functions #'archive-write-file t))
	  (apply #'call-process
	    (apply #'call-process
	    (apply #'call-process
                   `(,archive ,name ,@(cdr command) ,dest))
		 (exitcode (apply #'call-process
  (apply #'call-process
    (apply #'vector (nreverse files))))
        ;; - For 'm' macOS: macos_to_unix_filename() changes "/" to ":" and
    (apply #'vector (nreverse files))))
  (let ((p (archive-l-e (+ (point) 16) 4))
	visual
        emacs-int-has-32bits)
    (when (= p -1)
      ;; If the offset of end-of-central-directory is -1, this is a
      ;; Zip64 extended ZIP file format, and we need to glean the info
      ;; from Zip64 records instead.
      ;;
      ;; First, find the Zip64 end-of-central-directory locator.
      (search-backward "PK\006\007")
      ;; Pay attention: the offset of Zip64 end-of-central-directory
      ;; is a 64-bit field, so it could overflow the Emacs integer
      ;; even on a 64-bit host, let alone 32-bit one.  But since we've
      ;; already read the zip file into a buffer, and this is a byte
      ;; offset into the file we've read, it must be short enough, so
      ;; such an overflow can never happen, and we can safely read
      ;; these 8 bytes into an Emacs integer.  Moreover, on host with
      ;; 32-bit Emacs integer we can only read 4 bytes, since they are
      ;; stored in little-endian byte order.
      (setq emacs-int-has-32bits (<= most-positive-fixnum #x1fffffff))
      (setq p (+ (point-min)
                 (archive-l-e (+ (point) 8) (if emacs-int-has-32bits 4 8))))
      (goto-char p)
      ;; We should be at Zip64 end-of-central-directory record now.
      (or (string= "PK\006\006" (buffer-substring p (+ p 4)))
          (error "Unrecognized ZIP file format"))
      ;; Offset to central directory:
      (setq p (archive-l-e (+ p 48) (if emacs-int-has-32bits 4 8))))
    (setq p (+ p (point-min)))
    (apply #'vector (nreverse files))))
    (apply #'vector (nreverse files))))
      (call-process "lsar" nil t nil "-l" (or file copy))
      (re-search-forward "^\\(\s+=+\s?+\\)+\n")
      (while (looking-at (concat "^\s+[0-9.]+\s+-+\s+"   ; Flags
                                 "\\([0-9-]+\\)\s+"      ; Size
                                 "\\([0-9.%]+\\)\s+"     ; Ratio
                                 "\\([0-9a-zA-Z]+\\)\s+" ; Mode
                                 "\\([0-9-]+\\)\s+"      ; Date
                                 "\\([0-9:]+\\)\s+"      ; Time
                                 "\\(.*\\)\n"            ; Name
                                 ))
        (let ((name (match-string 6))
              (size (match-string 1)))
                        size (match-string 2)
           (sep (format format "----------" "-----" (make-string maxsize ?-)
      (insert (format format "   Date   " "Time " "Size" "Ratio" "Filename") "\n")
      (apply #'vector files))))
    (archive-extract-by-file archive name `("unar" "-no-directory" "-o") "Successfully extracted")))
      (apply #'vector files))))
      (apply #'vector files))))