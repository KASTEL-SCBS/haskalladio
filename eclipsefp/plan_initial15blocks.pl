%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                         %
%                            Initial   State                              %
%                                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                         %
%                         11                                              %
%           3             10                                              %
%           2              5          9                                   %
%           1              4          8                                   %
%          12             14          7                                   %
%          13             15          6                                   %
%    ----------------------------------------------------------------     %
%          16             17         18       19   20   21  22 .....      %
%                                                                         %
%                                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initially(13,16,0).
initially(12,13,0).
initially(1,12,0).
initially(2,1,0).
initially(3,2,0).

initially(15,17,0).
initially(14,15,0).
initially(4,14,0).
initially(5,4,0).
initially(10,5,0).
initially(11,10,0).


initially(6,18,0).
initially(7,6,0).
initially(8,7,0).
initially(9,8,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                         %
%                            Final   State (1)                            %
%                                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                         %
%                          7                                              %
%                          3                                              %
%                          6                                              %
%                         11                                              %
%                         10                                              %
%           2              5                                              %
%           1              4                                              %
%          12             14                                              %
%          13             15                                              %
%    -------------------------------------------------------------        %
%          16             17         18       19                          %
%                                                                         %
%                                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%           Query 1 :
%  
%  cputime(X), T :: 1..50, aclp_solve(planning([on(6, 11, T), on(7, 3, T),
%  on(3, 6, T)])), cputime(Y), CPUTIME is Y - X.
%
%
%               Answer (for Total Positions = 19):
%               ---------------------------------- 
%
%   T (Number of required moves) = 6
%
%   Solution = [move(3, 6, 5), not_moved(3, 0, 5, 6), move(7, 3, 6),
%   not_moved(7, 0, 6, 6), move(9, 19, 1), move(8, 9, 2), move(7, 8, 3),
%   move(6, 11, 4), not_moved(6, 0, 4, 6)]
%
%   CPUTIME = 1.27
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                         %
%                            Final   State (2)                            %
%                                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                         %
%                                                                         %
%                         15         12                                   %
%          14             13          2                                   %
%           1              8          3                                   %
%           5              9         11                                   %
%          10              4          7                                   %
%    ----------------------------------------------------------------     %
%          16             17         18       19   20                     %
%                                                                         %
%                                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      
%               Query:
%      
%      cputime(X), T :: 1..50, aclp_solve(planning([on(10, 16, T),
%                        on(5, 10, T), on(1, 5, T),
%                        on(14, 1, T), on(4, 17, T), on(9, 4, T), on(8, 9, T),
%                        on(13, 8, T), on(15, 13,T),
%                        on(7,18,T),on(11,7,T),on(3,11,T),on(2,3,T),
%                        on(12,2,T)])), cputime(Y), CPUTIME is Y - X.
%
%               Answer (for Total Positions = 20):
%               ---------------------------------- 
%
%   T (Number of required moves) = 26
%
%  Solution = [move(12, 2, 26), not_moved(12, 26, 26), move(2, 3, 25),
%  not_moved(2, 25, 26), move(12, 6, 22), move(2, 12, 23), move(3, 11, 24),
%  not_moved(3, 24, 26), move(11, 7, 21), not_moved(11, 21, 26),
%  move(7, 11, 18), move(6, 14, 19), move(7, 18, 20), not_moved(7, 20, 26),
%  move(15, 13, 17), not_moved(15, 17, 26), move(13, 8, 16),
%  not_moved(13, 16, 26), move(8, 9, 15), not_moved(8, 15, 26),
%  move(9, 4, 14), not_moved(9, 14, 26), move(15, 12, 12), move(4, 17, 13),
%  not_moved(4, 13, 26), move(4, 9, 10), move(14, 1, 11),
%  not_moved(14, 11, 26), move(1, 5, 9), not_moved(1, 9, 26), move(5, 10, 8),
%  not_moved(5, 8, 26), move(3, 19, 2), move(2, 3, 3), move(1, 9, 4),
%  move(12, 2, 5), move(13, 11, 6), move(11, 20, 1), move(10, 16, 7),
%  not_moved(10, 7, 26)]
%
%   CPUTIME = 268.44 (secs)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                         %
%                            Final   State (3)                            %
%                                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                         %
%                                                                         %
%                           12               15                           %
%                            2       14      13                           %
%                            3        1       8                           %
%                           11        5       9                           %
%                            7       10       4                           %
%    ------------------------------------------------------------         %
%          16       17      18       19      20      21    22             %
%                                                                         %
%                                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%               Query:
%  
% cputime(X), T :: 1..50,
%          aclp_solve(planning([on(10, 19, T), on(5, 10, T), on(1, 5, T),
%                        on(14, 1, T), on(4, 20, T), on(9, 4, T), on(8, 9, T),
%                        on(13, 8, T), on(15, 13,T),
%                        on(7,18,T),on(11,7,T),on(3,11,T),on(2,3,T),
%                        on(12,2,T)])), cputime(Y), CPUTIME is Y - X.
%
%               Answer (for Total Positions = 22):
%               ---------------------------------- 
%
%   T (Number of required moves) = 22
%
% Solution = [move(12, 2, 22), not_moved(12, 22, 22), move(2, 3, 21),
% not_moved(2, 21, 22), move(6, 15, 18), move(2, 6, 19), move(3, 11, 20),
% not_moved(3, 20, 22), move(11, 7, 17), not_moved(11, 17, 22), move(7, 8, 11),
% move(6, 2, 12), move(7, 18, 13), not_moved(7, 13, 22), move(15, 13, 16),
% not_moved(15, 16, 22), move(12, 14, 14), move(13, 8, 15),
% not_moved(13, 15, 22), move(8, 9, 10), not_moved(8, 10, 22), move(9, 4, 9),
% not_moved(9, 9, 22), not_moved(4, 7, 22), move(4, 20, 7), move(14, 1, 8),
% not_moved(14, 8, 22), move(3, 22, 4), move(2, 3, 5), move(1, 5, 6),
% not_moved(1, 6, 22), move(5, 10, 3), not_moved(5, 3, 22), move(11, 21, 1),
% move(10, 19, 2), not_moved(10, 2, 22)]
%
% CPUTIME = 211.75 (secs)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%