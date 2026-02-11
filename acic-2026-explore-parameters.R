# TODO: einlesen parameters_2016 file

# Linearity Treatment Assignment
sum(parameters_2016$model.trt == "linear") # 6
sum(parameters_2016$model.trt == "polynomial") # 39
sum(parameters_2016$model.trt == "step") # 32

# Overlap
sum(parameters_2016$overlap.trt == "full") # 32
sum(parameters_2016$overlap.trt == "one-term") # 45

# Linearity Response Surface
sum(parameters_2016$model.rsp == "linear") # 6
sum(parameters_2016$model.rsp == "exponential") #39
sum(parameters_2016$model.rsp == "step") # 32

