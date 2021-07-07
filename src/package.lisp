;; Copyright (c) 2020 Marin Atanasov Nikolov <dnaeon@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer
;;     in this position and unchanged.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage :cl-migratum
  (:nicknames :migratum)
  (:use :cl)
  (:import-from
   :cl-migratum.core
   :base-migration
   :migration-id
   :migration-description
   :migration-applied
   :migration-load-up-script
   :migration-load-down-script
   :base-provider
   :provider-init
   :provider-shutdown
   :provider-initialized
   :provider-name
   :provider-list-migrations
   :provider-create-migration
   :provider-find-migration-by-id
   :base-driver
   :driver-name
   :driver-provider
   :driver-init
   :driver-shutdown
   :driver-initialized
   :driver-list-applied
   :driver-register-migration
   :driver-unregister-migration
   :driver-apply-up-migration
   :driver-apply-down-migration
   :list-pending
   :latest-migration
   :display-pending
   :display-applied
   :apply-pending
   :contains-applied-migrations-p
   :apply-next
   :revert-last)
  (:import-from
   :cl-migratum.util
   :make-migration-id)
  (:export
   ;; core
   :base-migration
   :migration-id
   :migration-description
   :migration-applied
   :migration-load-up-script
   :migration-load-down-script
   :base-provider
   :provider-init
   :provider-shutdown
   :provider-initialized
   :provider-name
   :provider-list-migrations
   :provider-create-migration
   :provider-find-migration-by-id
   :base-driver
   :driver-name
   :driver-provider
   :driver-init
   :driver-shutdown
   :driver-initialized
   :driver-list-applied
   :driver-register-migration
   :driver-unregister-migration
   :driver-apply-up-migration
   :driver-apply-down-migration
   :list-pending
   :latest-migration
   :display-pending
   :display-applied
   :apply-pending
   :contains-applied-migrations-p
   :apply-next
   :revert-last

   ;; util
   :make-migration-id))
(in-package :cl-migratum)
