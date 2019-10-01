(fset 'comment-c-word
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([47 kp-multiply 32 134217848 134217840 134217840 return 91 44 41 93 return 2 32 42 47] 0 "%d")) arg)))
