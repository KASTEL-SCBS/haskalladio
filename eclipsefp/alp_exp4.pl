/* ************************** EX4 ********************************* */
/* This is an example with abducibles and integrity constraints.    */
/* Try this example with the following goals (G1 and G2):           */
/* G1: demo([car_doesnt_start(mycar)],[],FinalExp).                 */
/* G2: demo([car_doesnt_start(yourcar)],[],FinalExp).               */
/* Note that there are two alternative explanations for G2.         */
/* **************************************************************** */

:- dynamic ic/0,car_doesnt_start/1,lights_go_on/1,fuel_indicator_empty/1.


car_doesnt_start(X) :- battery_flat(X).
car_doesnt_start(X) :- has_no_fuel(X).


lights_go_on(mycar).
fuel_indicator_empty(mycar).


/* Integrity Constraints */

ic :- battery_flat(X), lights_go_on(X).

ic :- has_no_fuel(X), not(fuel_indicator_empty(X)), not(broken_indicator(X)).

/* This second constraint is equivalent to : */
/* fuel_indicator_empty(X):- has_no_fuel(X), not(broken_indicator(X)). */
/* written as a denial. */


/* Abducibles */

abducible_predicate(battery_flat).
abducible_predicate(has_no_fuel).
abducible_predicate(broken_indicator).
