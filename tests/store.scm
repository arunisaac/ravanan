;;; ravanan --- High-reproducibility CWL runner powered by Guix
;;; Copyright © 2025 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of ravanan.
;;;
;;; ravanan is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; ravanan is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with ravanan.  If not, see <https://www.gnu.org/licenses/>.

(use-modules (srfi srfi-64)
             (ravanan store))

(test-begin "store")

(test-equal "step-store-files-directory must be insensitive to order of inputs"
  (step-store-files-directory "/gnu/store/hl96c0xd19ngvl8rf4cyw452rpgqsi1b-foo"
                              '(("foo" . 1)
                                ("foobar" . 3)
                                ("bar" . (("aal" . 1)
                                          ("vel" . 2))))
                              "store")
  (step-store-files-directory "/gnu/store/hl96c0xd19ngvl8rf4cyw452rpgqsi1b-foo"
                              '(("foo" . 1)
                                ("bar" . (("vel" . 2)
                                          ("aal" . 1)))
                                ("foobar" . 3))
                              "store"))

(test-equal "step-store-data-file must be insensitive to order of inputs"
  (step-store-data-file "/gnu/store/hl96c0xd19ngvl8rf4cyw452rpgqsi1b-foo"
                        '(("foo" . 1)
                          ("foobar" . 3)
                          ("bar" . (("aal" . 1)
                                    ("vel" . 2))))
                        "store")
  (step-store-data-file "/gnu/store/hl96c0xd19ngvl8rf4cyw452rpgqsi1b-foo"
                        '(("foo" . 1)
                          ("bar" . (("vel" . 2)
                                    ("aal" . 1)))
                          ("foobar" . 3))
                        "store"))

(test-equal "step-store-stdout-file must be insensitive to order of inputs"
  (step-store-stdout-file "/gnu/store/hl96c0xd19ngvl8rf4cyw452rpgqsi1b-foo"
                          '(("foo" . 1)
                            ("foobar" . 3)
                            ("bar" . (("aal" . 1)
                                      ("vel" . 2))))
                          "store")
  (step-store-stdout-file "/gnu/store/hl96c0xd19ngvl8rf4cyw452rpgqsi1b-foo"
                          '(("foo" . 1)
                            ("bar" . (("vel" . 2)
                                      ("aal" . 1)))
                            ("foobar" . 3))
                          "store"))

(test-equal "step-store-stderr-file must be insensitive to order of inputs"
  (step-store-stderr-file "/gnu/store/hl96c0xd19ngvl8rf4cyw452rpgqsi1b-foo"
                          '(("foo" . 1)
                            ("foobar" . 3)
                            ("bar" . (("aal" . 1)
                                      ("vel" . 2))))
                          "store")
  (step-store-stderr-file "/gnu/store/hl96c0xd19ngvl8rf4cyw452rpgqsi1b-foo"
                          '(("foo" . 1)
                            ("bar" . (("vel" . 2)
                                      ("aal" . 1)))
                            ("foobar" . 3))
                          "store"))

(test-end "store")
