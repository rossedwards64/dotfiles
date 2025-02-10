{ pkgs }:

pkgs.writers.writeGuileBin "toggle-mute" { } ''
  (use-modules (ice-9 getopt-long)
               (ice-9 popen)
               (ice-9 rdelim))

  (define (usage)
    "Script for muting the speaker or microphone.
  Usage: toggle-mute [-ms | -h]
  m: Toggle microphone mute.
  s: Toggle speaker mute.
  h: Print usage")

  (define (toggle-device device id)
    (system (format #f "${pkgs.wireplumber}/bin/wpctl set-mute '~a' toggle" id))
    (let* ((pipe (open-input-pipe (format #f
                                   "${pkgs.wireplumber}/bin/wpctl get-volume '~a'"
                                   id)))
           (output (read-line pipe)))
      (close-pipe pipe)
      (if (string-contains output "[MUTED]")
          (system (format #f "${pkgs.libnotify}/bin/notify-send \"Muted ~a\"" device))
          (system (format #f "${pkgs.libnotify}/bin/notify-send \"Unmuted ~a\"" device)))))

  (let* ((option-spec '((microphone (single-char #\m) (value #f))
                        (speaker (single-char #\s) (value #f))
                        (help (single-char #\h) (value #f))))
         (options (getopt-long (command-line) option-spec))
         (microphone (option-ref options 'microphone #f))
         (speaker (option-ref options 'speaker #f)))
    (if (not (nil? (option-ref options 'help #f)))
        (begin
          (display (usage))
          (newline))
        (letrec ((devices (filter (compose not nil?)
                                  (list (if (nil? microphone)
                                            #f
                                            '("microphone" . "@DEFAULT_AUDIO_SOURCE@"))
                                        (if (nil? speaker)
                                            #f
                                            '("speaker" . "@DEFAULT_AUDIO_SINK@")))))
                 (toggle-devices (lambda (devices)
                                   (unless (nil? devices)
                                     (toggle-device (caar devices) (cdar devices))
                                     (toggle-devices (cdr devices))))))
          (if (nil? devices)
              (error "Must specify the microphone and/or speaker.\n")
              (toggle-devices devices)))))
''
