;;
;; Copyright 2022, Jaidyn Levesque <jadedctrl@posteo.at>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;


(import scheme
		(chicken io) (chicken tcp)
		srfi-1 srfi-69 srfi-130
		openssl)


;; Connect to the given IRC server, returning an IRC connection object.
(define (irc:connect host port username nick #!optional (password #f) (realname #f))
  (let ([conn (make-hash-table)])
	(define-values (out in)
	  (ssl-connect* hostname: host port: port))
	(hash-table-set! conn 'in in)
	(hash-table-set! conn 'out out)

	(if password
		(irc:write-cmd conn "PASS" password))
	(irc:write-cmd conn "USER" username "*" "0"
				   (if realname realname "Jane Row"))
	(irc:write-cmd conn "NICK" nick)
	conn))


;; Join the given channel. Not much to say, really.
(define (irc:join conn channel #!optional key)
  (let* ([params-sans-key (list conn "JOIN" channel)]
		 [params (if key (append params-sans-key `(,key))
					 params-sans-key)])
		(apply irc:write-cmd params)))


;; Read-in the next reply or command from the server, into a parsable alist with
;; four keys:
(define (irc:read-alist conn)
  (irc:process-alist-internally
   conn
   (irc:line->alist
	(read-line (hash-table-ref conn 'out)))))


(define (irc:read-line conn)
  (read-line (hash-table-ref conn 'out)))


;; Send a specific command to the server.
(define (irc:write-cmd conn command . parameters)
  (irc:write-line (apply irc:cmd->string (append `(,command) parameters))
				  conn))


;; Write a line to the IRC server connection.
(define (irc:write-line text connection)
  (write-line text (hash-table-ref connection 'in)))


;; The user should have more-or-less total control over how to respond to
;; received messages, but ircc has to sneakily process some responses itself,
;; to ensure basic functionality (i.e., pings, chanlist, userlist, etc.)
(define (irc:process-alist-internally conn alist)
  (let ([command (alist-car-ref 'command alist)]
		[reply (alist-car-ref 'reply alist)]
		[sender (alist-car-ref 'sender alist)]
		[params (alist-ref 'params alist)])
	(if command
		(irc:process-command-internally conn command params sender)
		(irc:process-reply-internally conn reply params sender)))
  alist)


;; Handle some replies necssary for basic functionality
(define (irc:process-reply-internally conn reply params #!optional sender)
  (cond [(eq? reply 001)
		 (hash-table-set! conn 'registered #t)]))


;; Handle some commands necessary for basic funcitonality
(define (irc:process-command-internally conn command params #!optional sender)
  (cond [(string=? command "PING")
		 (irc:write-cmd conn "PONG" (last params))]))


;; Construct a string to write to IRC for the given command and parameters.
(define (irc:cmd->string command . parameters)
  (let ([parameters
		 (append (reverse (cdr (reverse parameters)))
				 `(,(string-append ":" (last parameters))))])
	(string-append
	 command
	 " "
	 (reduce-right
	  (lambda (a b)
		(string-append a " " b))
	  #f
	  parameters))))


;; Convert a string to a `msg` alist, with keys 'command', 'reply', 'params',
;; and 'sender'.
(define (irc:line->alist str)
  (let* ([colon-split (string-split str " :")]
		 [last-column (reduce string-append #f (cdr colon-split))] ;; for additional colons post-colon
		 [other-columns (string-split (car colon-split) " ")]
		 [sender (if (eq? #\:
						 (car (string->list (car other-columns))))
					 (string-drop (car other-columns) 1)
					 #f)]
		 [command (if sender
					  (cadr other-columns)
					  (car other-columns))]
		 [reply (string->number command)]
		 [params (append (if sender (cddr other-columns) (cdr other-columns))
						 (list last-column))])
	`((command ,(if (not reply) command #f))
	  (reply ,reply)
	  ,(append '(params) params)
	  (sender ,sender))))

;; Just car's the value of alist-ref (if it exists)
(define (alist-car-ref key alist)
  (let ([value (alist-ref key alist)])
	(if value
		(car value)
		#f)))
