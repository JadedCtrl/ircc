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


(import srfi-1 openssl (chicken tcp) (chicken io))


;; Connect to the given IRC server, returning an IRC connection object.
(define (irc:connect host port username nick #!optional (password #f) (realname #f))
  (define-values (out in)
	(ssl-connect* hostname: host port: port))

  (let ([conn (list out in)])
	(if password
		(irc:write-cmd conn "PASS" password))
	(irc:write-cmd conn "USER" username "*" "0"
				   (if realname realname "Jane Row"))
	(irc:write-cmd conn "NICK" nick)
	conn))


;; Send a specific command to the server.
(define (irc:write-cmd conn command . parameters)
  (irc:write-line (apply irc:cmd (append `(,command) parameters))
				  conn))


;; Construct a string to write to IRC for the given command and parameters.
(define (irc:cmd command . parameters)
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


;; Join the given channel. Not much to say, really.
(define (irc:join conn channel #!optional key)
  (let* ([params-sans-key (list conn "JOIN" channel)]
		 [params (if key (append params-sans-key `(,key))
					 params-sans-key)])
	(apply irc:write-cmd params)))


;; Write a line to the IRC server connection.
(define (irc:write-line text connection)
  (print text)
  (write-line text (cadr connection)))
