obj_to_save <- c(
  'train',
  'validation',
  'lasso',
  'best_lambda',
  'numeric_vars',
  'dummy',
  'design_matrix'
)

save(list = obj_to_save, file = 'R_obj/R_objects.RData')
