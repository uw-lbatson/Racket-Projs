# Racket-Projs

These are a variety of Racket applications that do a variety of different tasks.

Brackets: This application uses the basic idea of stacks to determine if a given string of brackets are 'balanced'. 
That is, the brackets have an opening bracket for every closing bracket.

Change: This application uses the idea of dynamic programming to determine the smallest number of coins that equal a given total using a given currency.
This problem was solved using dynamic programming rather than the typical greedy method as it is much more effective.

As an example, we consider the currency of 1 cent, 9 cents and 19 cents. How would get the least number of coins for 27 cents? A greedy algorithm would start at the 
largest currency, subtract it from the total, and determine how to get the smallest amount from that total. This method would give us the coins: 19, 1, 1, 1, 1, 1, 1, 1, 1.
We can clearly see however, that we could simply have three coins: 9, 9, 9. We can see that the greedy algorithm is wrong in this case, which is why it is best to use another method.
The application has been optimized to have a big O value of O(n*log2(n)).

Guess: This application uses the basic idea of machine learning to build decision trees that determine if given animal characteristics are an animal. Both files, animal and guess,
are needed to run the application.
