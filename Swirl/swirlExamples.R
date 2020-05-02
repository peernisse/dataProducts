#'swirl notes and example code
#'thisis for week for of class 9
#'THe purpose here is to create our own swirl lesson(s)

#install.packages('swirlify')
library(swirlify)

new_lesson("Lesson 1","My First Swirl Course")
wq_message()
wq_command()
add_to_manifest()
test_lesson()

#New lesson
get_current_lesson()#Checking what lesson we are on and in what course
#Start new lesson in same course
new_lesson("Lesson 2", "My First Swirl Course")
#Make multiple choice question
wq_multiple()#Then go edit the question in the YAML file
add_to_manifest()
test_lesson()
demo_lesson()


#Make figure question
wq_figure()#Then go edit the YAML
#Made a figure file .R
demo_lesson()

#Make another figure question so user can work on it
wq_figure()#Editied another figure file and selected type as 'add' instead of 'new'
test_lesson()
demo_lesson()




