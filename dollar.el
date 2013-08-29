;;; dollar.el --- Gauche envy; Haskell-ish application

;; Copyright (C) 2013 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Keywords: lisp, extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Gauche envy;
;; from Gauche's (http://practical-scheme.net/gauche/) common-macro.scm,
;; ---8<---8<---8<---

;;;
;;; common-macros.scm - common macros
;;;
;;;   Copyright (c) 2000-2012  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;;;-------------------------------------------------------------
;;; applications

;; Haskell-ish application.
;; The starting '$' introduces the macro.
;; Subsequent '$' delimits "one more arguments"
;; Subsequent '$*' delimits "zero or more arguments".
;;
;;  ($ f a b c)         => (f a b c)
;;  ($ f a b c $)       => (lambda (arg) (f a b c arg))
;;  ($ f $ g a b c)     => (f (g a b c))
;;  ($ f $ g a b c $)   => (lambda (arg) (f (g a b c arg)))
;;  ($ f $ g $ h a b c) => (f (g (h a b c)))
;;  ($ f a $ g b $ h c) => (f a (g b (h c)))
;;  ($ f a $ g b $ h $) => (lambda (arg) (f a (g b (h arg))))
;;
;;  ($ f a b c $*)      => (lambda args (apply f a b c args))
;;                         == (pa$ f a b c)
;;  ($ f a b $* g c d)  => (apply f a b (g c d))
;;  ($ f a b $* g c d $) => (lambda (arg) (apply f a b (g c d arg)))
;;  ($ f a b $* g c d $*) => (lambda args (apply f a b (apply g c d args)))
;;  ($ f a b $ g c d $*) => (lambda args (f a b (apply g c d args)))

;; --->8--->8--->8---
;; It sounds quite promising, so I port it to emacs lisp.

;;; Example:
;;
;;  ($ f a b c)         ⇒ (f a b c)
;;  ($ f a b c $)       ⇒ (lambda (arg) (f a b c arg))
;;  ($ f $ g a b c)     ⇒ (f (g a b c))
;;  ($ f $ g a b c $)   ⇒ (lambda (arg) (f (g a b c arg)))
;;  ($ f a $ g b $ h c) ⇒ (f a (g b (h c)))
;;  ($ f a $ g b $ h $) ⇒ (lambda (arg) (f a (g b (h arg))))
;;
;;  ($ f a b c $*)      ⇒ (lambda (&rest args) (apply f a b c args))
;;  ($ f a b $* g c)    ⇒ (apply f a b (g c))
;;  ($ f a b $* g c $)  ⇒ (lambda (arg) (apply f a b (g c arg)))
;;  ($ f a b $* g c $*) ⇒ (lambda (&rest args) (apply f a b (apply g c args)))
;;  ($ f a b $ g c $*)  ⇒ (lambda (&rest args) (f a b (apply g c args)))
;;
;;  And experimental stuff; '$&' is designated as `funcall'
;;  ($ f $& g a b c)    ⇒ (f (funcall g a b c))
;;  ($ f $& g a b c $)  ⇒ (lambda (arg) (f (funcall g a b c arg)))
;;  ($ f $& g a b c $*) ;; TODO: yields error? currently same as below
;;  ($ f $ g a b c $*)  ⇒ (lambda (&rest args) (f (apply g a b c args)))
;;

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'cl-macs))

(defmacro $ (&rest form)
  "Haskell-ish application"
  ($-expand `($ ,@form)))

(defun $-expand (s)
  (pcase s
    (`($) (error "invalid $ form: `%S'" s))
    (`($ . ,ss) ($-split ss nil nil))))

;; TODO: `cl-gensym' will be `autoload'ed, so it seems ok to suppress the
;; "the function `cl-gensym' might not be defined at runtime" warning.
;;(declare-function cl-gensym "cl-macs" (cl-gensym &optional prefix) t)
;; or just defalias does the job like this?
(defalias '$-gensym 'cl-gensym)

(defun $-split (ss segs form)
  (macrolet ((w/uniq (names &rest body) ;; borrowed from arc
               `(let ,(mapcar (lambda (s) `(,s ($-gensym ,(symbol-name s))))
                              (if (consp names) names (list names)))
                  ,@body)))
    (w/uniq (garg gargs)
      (pcase ss
        (`()   ($-gen 'knil           `((,@form)              ,@segs)))
        (`($)  ($-gen `(,garg)        `((,@form ,garg)        ,@segs)))
        (`($*) ($-gen `(&rest ,gargs) `((apply ,@form ,gargs)
                                        ,@(pcase segs
                                            (`($& . ,tail) `($ ,@tail))
                                            (_ segs)))))
        (`(,(and (or `$ `$* `$&) type) . ,tail)
          ($-split tail `(,type ,form ,@segs) nil))
        (`(,head . ,tail)
          ($-split tail segs `(,@form ,head)))))))

(defun $-gen (type specs)
  (pcase `(,type . ,specs)
    (`(,type . (,seg0 . ($ . (,s . ,segs))))
      ($-gen type `((,@s ,seg0) ,@segs)))
    (`(,type . (,seg0 . ($* . (,s . ,segs))))
      ($-gen type `((apply ,@s ,seg0) ,@segs)))
    (`(,type . (,seg0 . ($& . (,s . ,segs))))
      ($-gen type `((,@s (funcall ,@seg0)) ,@segs)))
    (`(knil . (,seg)) seg)
    (`(,formal . (,seg)) `(lambda ,formal ,seg))
    (s (error "$-gen internal error: `%S'" s))))

(provide 'dollar)
;;; dollar.el ends here
