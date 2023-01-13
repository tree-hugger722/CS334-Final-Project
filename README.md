# CS334-Final-Project

(c) Emma Neil, Anna Owens 2022

Welcome to the repository for The Tosser, a domain-specific programming language to generate seasonal salads according to the principles of Josh McFaden and Martha Holmberg's cookbook. We build The Tosser as an independent final project for Professor Dan Barowy's Principles of Programming Languages course in the Fall 2022 semester at Williams College.

### Play with the Salad Generator ###
To start writing programs to generate recipes, clone the repository and run the following command:

`dotnet run`

The program will offer you the option to review a list of syntax rules for writing your program on the command line. Happy tossing!

### For the Interested User... ###
The Tosser programming language was written in the functional programming language F#. It consists of a program called a Parser which takes in simple string inputs on the command line and outputs an abstract syntax tree. A second program, called an Evaluator, takes in an abstract syntax tree and outputs a list of ingredients for a salad. 

For more information on the abstract syntax tree and design choices we made in creating the language, check out the recording of our final presentation of this project at the following Google Drive link (you may have to download the video into order to watch it): https://drive.google.com/file/d/1bxOcT3I7vrn0csMQJhWUqTU9JM7Rq21I/view?usp=sharing
