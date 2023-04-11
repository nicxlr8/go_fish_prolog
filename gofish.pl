%! https://gist.github.com/trietptm/4277978 
%! appendlist(L, L1, L2) is true if L2 is the list L appended to the list L1
appendlist([], X, X).
appendlist([H|T], L1, [H|L2]) :- appendlist(T, L1, L2).

%! https://gist.github.com/trietptm/4277978 
%! dupli(L, M, L1) is true if L1 is the list L duplicated M times
dupli([], _, []) :-!.
dupli([X], 1, [X]) :-!.
dupli([X], M, [X|L]) :- N is M - 1, dupli([X], N, L), !.
dupli([H|T], M, L1) :- dupli([H], M, H1), dupli(T, M, T1), appendlist(H1, T1, L1), !.

%! shuffle(DECK, S) is true if S is the list DECK but shuffled
shuffle(DECK, S) :-
dupli(["2","3","4","5","6","7","8","9","10","J","Q","K","A"],4,DECK),
random_permutation(DECK,S).

%! start_state initializes the deck and hand of both players
start_state(SD, [], [], PH8, CH8, SD15) :- shuffle(_, SD), draw(SD, [], PH2, SD2), draw(SD2, [], CH2, SD3),
    draw(SD3, PH2, PH3, SD4), draw(SD4, CH2, CH3, SD5),
    draw(SD5, PH3, PH4, SD6), draw(SD6, CH3, CH4, SD7),
    draw(SD7, PH4, PH5, SD8), draw(SD8, CH4, CH5, SD9),
    draw(SD9, PH5, PH6, SD10), draw(SD10, CH5, CH6, SD11),
    draw(SD11, PH6, PH7, SD12), draw(SD12, CH6, CH7, SD13),
    draw(SD13, PH7, PH8, SD14), draw(SD14, CH7, CH8, SD15).
    
%! draw(L, HAND, NEWHAND, R) is true if NEWHAND is HAND with the head of L appended to it and R is the rest of L
draw([H|T], HAND, NEWHAND, T) :- append([H], HAND, NEWHAND).

%! remove(X,Y,P,PR,F) is true if remove_duplicates(X,Y,P,PR,F) is true
remove(X,Y,P,PR,F) :- remove_duplicates(X,Y,P,PR,F).

%! remove_duplicates(X,Y,P,PR,F) is true if Y is the list X without any duplicates and PR is P + the number of duplicates that were removed
remove_duplicates([], [], Total, Total, _).

remove_duplicates([H | T], R, PNum, Total, F) :-
    member(H, T), !,
    PNum2 is PNum + 1,
    delete_one(H,[H | T], R2),
    delete_one(H, R2, R3),
    (F == 0 -> writeln("You've added a pair to your pair pile!") ; 
        writeln("The computer added a pair to its pair pile.")
        ),
    remove_duplicates(R3, R, PNum2, Total, F).

remove_duplicates([H | T], [H | R], PNum, Num, F) :-
    \+ member(H, T),
    remove_duplicates(T, R, PNum, Num, F).
    
/*
https://stackoverflow.com/questions/15857382/removing-first-occurrence-in-list-prolog
*/
%! delete_one(X,L,L2) is true if L2 is the list L with the first occurence of X removed
delete_one(X,[X|T],T):-!.
delete_one(X,[Y|T],[Y|T1]):-delete_one(X,T,T1).


%! ask_card is true if the user inputs a valid card
ask_card(SD, CURRPH, OPPPH, SD2, CURRPH2, OPPPH2, TURN) :-
    repeat,
        writeln("To ask the computer if they have a card enter the position of a card in your hand"),
        read_line_to_string(user_input, String), 
        (member(String,CURRPH) -> check_opp(String, SD, CURRPH, OPPPH, SD2, CURRPH2, OPPPH2, TURN),! ; fail 
        ).

%! check_opp is true if it correctly changes both players hands and the deck
check_opp(String, SD, CURRPH, OPPPH, SD, CURRPH2, OPPPH2, _) :-
    member(String, OPPPH),
    append([String], CURRPH, CURRPH2),
    delete(OPPPH, String, OPPPH2).
    
check_opp(String, [H|T], CURRPH, OPPPH, T, CURRPH2, OPPPH, TURN) :-
    \+ member(String, OPPPH),
    append([H], CURRPH, CURRPH2),
    (TURN == 0 -> write("You drew a "), write(H), write("!"), nl ;
        writeln("The computer drew a card.")
    ).

%! computer_turn is true if it correctly performs the computer's turn
computer_turn(SD, PH, [H|T], PP, _, _, _, CP, F) :-
    write("Computer asks if you have a "), write(H), write(" in your hand"), nl,
    (F == 0 -> check_opp(H, SD, [H|T], PH, SDRR, CHRR, PHRR, 1) ; 
        good_bot(SD, [H|T], PH, SDRR, CHRR, PHRR)
        ),
    remove(CHRR, CHRRR, CP, CPR, 1),
    length(CHRRR, L),
    write('The computer has '), write(L), write(' cards in their hand.'), nl,
%!    writeln(CHRRR),
      
    (\+ end_game(SDRR, PHRR, CHRRR) -> player_turn(SDRR, PHRR, CHRRR, PP, _, _, _, CPR, F) ;
        writeln("The game has ended."),  
        write("You have "), write(PP),  write(" pairs in your pair pile!"), nl,
        write("The computer has "), write(CPR),  write(" pairs in its pair pile."), nl, 
        check_winner(PP, CPR)
        ).
        
%! good_bot(SD, [H|T], PH, SDRR, CHRRR, PHRR) is true if CHRRR is the list CHRR with H appended to the end of it
good_bot(SD, [H|T], PH, SDRR, CHRRR, PHRR) :-
    delete_one(H, [H|T], DLT),
    check_opp(H, SD, DLT, PH, SDRR, CHRR, PHRR, 1),
    append(CHRR, [H], CHRRR).
    
%! player_turn is true if it correctly performs the player's turn  
player_turn(SD, PH, CH, PP, _, _, _, CP, F) :-
    writeln('Here is your hand:'),
    writeln(PH),
%!    writeln('Here is the deck:'),
%!    writeln(SD),
    ask_card(SD, PH, CH, SD2, PH2, CH2, 0),
    remove(PH2, PH3, PP, PPR, 0),
    writeln('Here is your hand:'),
    writeln(PH3),nl,
    (\+ end_game(SD2, PH3, CH2) -> computer_turn(SD2, PH3, CH2, PPR, SD2, _, _, CP, F) ;
        writeln("The game has ended."),    
        write("You have "), write(PPR),  write(" pairs in your pair pile!"), nl,
        write("The computer has "), write(CP),  write(" pairs in its pair pile."), nl,
        check_winner(PPR, CP)
        ).

%! check_winner(PP, CP) is true if it correctly determines the winner by comparing PP and CP
check_winner(PP, CP) :-
    PP == CP,
    writeln("It's a tie!").

check_winner(PP, CP) :-
    PP > CP,
    writeln("You won!").

check_winner(PP, CP) :-
    PP < CP,
    writeln("You lost!!!").

%! end_game(D,H,OH) is true if either hand has no cards or the deck has no cards based on D, H, and OH.    
end_game(_, H, _) :- 
    length(H, L), L == 0.

end_game(D, _, _) :- 
    length(D, L), L == 0.
    
end_game(_, _, OH) :- 
    length(OH, L), L == 0.
    

main :- writeln('Welcome to our Go Fish game!'),
    pick_bot(CHOICE),
    start_state(_, _, _, PH8, CH8, SD15),
    writeln('Here is your hand:'),
    writeln(PH8),
    remove(PH8, PH9, 0, PP, 0),
    remove(CH8, CH9, 0, CP, 1),nl,
%!    writeln('Here is computers hand:'),
%!    writeln(CH9),
    player_turn(SD15, PH9, CH9, PP, _, _, _, CP, CHOICE).
    
%! pick_bot(CHOICE) is true if CHOICE is the user input
pick_bot(CHOICE):-
    writeln("Enter 0 to play the easy bot, or anything else to play the hard bot."),
    read_line_to_string(user_input, String), 
    (String=="0" -> CHOICE is 0 ; CHOICE is 1 
        ).


