;;;; Modeling 2 (non-delayed)

(define (3d-ll-model->painter 3d-ll-model)
  (let* ((2dll (3d-ll-model->2d-ll-model 3d-ll-model *camera* *lights*)))
    (2d-ll-model->painter 2dll)))
