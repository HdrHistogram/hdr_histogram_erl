;;;;
;;;; Simple histogram capture example using LFE (Lisp flavored Erlang)
;;;;

(defun record (r n)
    (case n
        (0 'ok)
        (n 
            (hdr_histogram:record r (random:uniform n))
            (record r (- n 1)))))

(defun main (args)
    ;; Create a fresh HDR histogram instance
    (let ((r (case (hdr_histogram:open 10000000 3) ((tuple 'ok x) x))))

    ;; record a random uniform distribution of 1M data points
    (record r 100000)

    ;; print percentiles to stdout as CLASSIC
    (hdr_histogram:print r 'classic)
    (timer:sleep 1000)

    ;; log percentiles to file as CSV
    (hdr_histogram:log r 'csv "elixir.hgrm")

    ;; print other values
    ; (let (
    ;     (min (hdr_histogram:min r))
    ;     (mean (hdr_histogram:mean r))
    ;     (median (hdr_histogram:median r))
    ;     (max (hdr_histogram:max r))
    ;     (stddev (hdr_histogram:stddev r))
    ;     (99le (hdr_histogram:percentile r 99.0))
    ;     (999999le (hdr_histogram:percentile r 99.9999))
    ;     (mem (hdr_histogram:get_memory_size r))
    ;     (count (hdr_histogram:get_total_count r)))
    ;     ; do something with the values ...
    
    ;; we're done, cleanup any held resources
    (hdr_histogram:close r)

    (io:format "Done!~n")))
