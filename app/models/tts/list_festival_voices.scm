(define (print_voice voice)
  (if voice
    (let (
           (name (car voice))
           (desc (cadr voice))
         )
         (let (
                (language (assoc 'language desc))
                (gender (assoc 'gender desc))
                (dialect (assoc 'dialect desc))
              )
              (format t "%s|%s|%s|%s\n" name (cadr language) (cadr gender) (cadr dialect))
         )
    )
  )
)

(define (print_voices voices)
  (while voices
    (print_voice (voice.description (car voices)))
    (set! voices (cdr voices))
  )
)

(print_voices (voice.list))
