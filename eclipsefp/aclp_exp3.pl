/* ************************** EX3 ********************************* */
/* This is an example with NAF through abduction and                */
/* other abducibles (no integrity constraints).                     */
/* We assume here that our knowledge about birds is incomplete      */
/* and so we define a new abducible "bird_ab(X)" which states       */
/* that X is a bird. Also "slim" is an abducible predicate.         */
/* Try this example with the following goals (G1, G2 and G3):       */
/* G1: demo([flies(tweety)],[],FinalExp).                           */
/* G2: demo([flies(sam)],[],FinalExp).                              */
/* G3: demo([flies(john)],[],FinalExp).                             */
/* Note that now only the first goal (G1) fails.                    */
/* Note also that the second goal (G2) now has two explanations.    */
/* **************************************************************** */

:- dynamic flies/1,abnormal/1,heavy/1,light/1,bird/1,bird_ab/1,slim/1,penguin/1,eagle/1, ic/0.


flies(X) :- bird(X), not(abnormal(X)).

abnormal(X) :- penguin(X).
abnormal(X) :- heavy(X).

heavy(X) :- not(light(X)).

light(X) :- slim(X).

bird(X) :- penguin(X).
bird(X) :- eagle(X).
bird(X) :- bird_ab(X).

penguin(tweety).
eagle(sam).

ic:- bird_ab(X).

/* Abducibles */

abducible_predicate(bird_ab/1).
abducible_predicate(slim/1).
