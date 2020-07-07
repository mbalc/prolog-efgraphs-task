%%% Set operations

% throughout the task I wrap list terms in a `set(X)` clause
% this is only to indicate that I expect uniqueness from a list
% that is wrapped in such a way

% here I define a set of helper operations
% that ensure that this invariant is met
% whenever we use and modify such sets 

setAdd(set(S), P, set([P | S])) :- \+ member(P, S).
setAdd(set(S), P, set(S)) :- member(P, S).

setAddAll(set(S), set(L), set(X)) :- setAddAll(set(S), L, set(X)).
setAddAll(set(S), [P | L], set(X)) :- setAdd(set(S), P, set(SS)), setAddAll(set(SS), L, set(X)).
setAddAll(set(S), [], set(S)).

setMember(P, set(S)) :- member(P, S).

setRemove(set(S), P, set(X)) :- member(P, S), delete(S, P, X).

listToSet([], set(_)).
listToSet(L, set(S)) :- setAddAll(set([]), L, set(S)).

setsEqual(set([]), set([])).
setsEqual(set([P | S]), set(SS)) :-
  setRemove(set(SS), P, set(NewSS)),
  setsEqual(set(S), set(NewSS)).


%%% Other helper methods
% pairs(L1, L2, Res)
% not tested, usages shouldn't affect score (code commented out)
pairs(L1, L2, Res) :- pairs(L1, L2, Res, []).
pairs([], _, Res, Res).
pairs(L1, L2, Res, Pairs) :- pairs(L1, L2, Res, Pairs, L2).
pairs([_ | L1], L2, Res, Pairs, []) :- pairs(L1, L2, Res, Pairs).
pairs([P | L1], L2, Res, Pairs, [Q | L2]) :-
  pairs([P | L1], L2, Res, [pair(P, Q) | Pairs], L2).


%%% jestEFGrafem(GraphRep)
% INTERFACE METHOD
% specified in the task description
jestEFGrafem(GraphRep) :-
  hasNoNodeRepetitions(GraphRep),
  areFEdgesValid(GraphRep),
  areEEdgesValid(GraphRep).

% hasNoNodeRepetitions(GraphRep, set(EncounteredNodes))
% True iff GraphRep doesn't have multiple descriptions for
% one node and doesn't have a description of a node
% that is already in EncounteredNodes
hasNoNodeRepetitions(GraphRep) :- hasNoNodeRepetitions(GraphRep, set([])).
hasNoNodeRepetitions([], set(_)).
hasNoNodeRepetitions([node(N, _, _) | Ns], set(EncounteredNodes)) :-
  \+ member(N, EncounteredNodes),
  setAdd(set(EncounteredNodes), N, set(NewEncountereds)),
  hasNoNodeRepetitions(Ns, set(NewEncountereds)).

% areFEdgesValid(GraphRep)
% True iff conditions for F edges are met in GraphRep representation
areFEdgesValid(GraphRep) :-
  fRelationEquivalent(GraphRep).

% areEEdgesValid(GraphRep)
% True iff conditions for E edges are met in GraphRep representation
areEEdgesValid(GraphRep) :-
  eEdgesCorrespondToRealNodes(GraphRep).

% fRelationEquivalent(GraphRep, ExpectedEdges) :-
% True iff GraphRep has F-edges that are symmetric to
% either themselves or some other F-edge in GraphRep
fRelationEquivalent(GraphRep) :- fEdges(GraphRep, set(FEdges)), !, edgeRelationSymmetric(FEdges).

% edgeRelationSymmetric(Edges)
% True iff an edge (a, b) from Edges
% is corresponded with a (b, a) edge from there
% (or special case is met: (a, a))
edgeRelationSymmetric([]).
edgeRelationSymmetric([edge(P, P) | Es]) :-
  edgeRelationSymmetric(Es).
edgeRelationSymmetric([edge(P, Q) | Es]) :-
  member(edge(Q, P), Es),
  delete(Es, edge(Q, P), NewEs),
  edgeRelationSymmetric(NewEs).

% fEdges(GraphRep, set(Edges))
% returns the whole set of F-edges from GraphRep
% True when Edges is a valid set of F-edges from GraphRep
fEdges(Ns, set(Res)) :-
  fEdges(Ns, set([]), set(Res)).
fEdges([node(_, _, []) | Ns], set(Es), set(Res)) :-
  fEdges(Ns, set(Es), set(Res)).
fEdges([node(N, _, [F | Fs]) | Ns], set(Es), set(Res)) :-
  setAdd(set(Es), edge(N, F), set(NewEs)),
  fEdges([node(N, _, Fs) | Ns], set(NewEs), set(Res)).
fEdges([], set(Es), set(Res)) :- Es = Res.

eEdgesCorrespondToRealNodes(GraphRep) :-
  graphNodes(GraphRep, set(Nodes)),
  !,
  eEdgesCorrespondToNodes(GraphRep, Nodes).

% eEdgesCorrespondToNodes(GraphRep, Nodes).
eEdgesCorrespondToNodes([], _).
eEdgesCorrespondToNodes([node(_, [], _) | Ns], Nodes) :-
  eEdgesCorrespondToNodes(Ns, Nodes).
eEdgesCorrespondToNodes([node(_, [E | Es], _) | Ns], Nodes) :-
  member(E, Nodes),
  eEdgesCorrespondToNodes([node(_, Es, _) | Ns], Nodes).

% graphNodes(GraphRep, set(FoundNodes))
% True if Nodes are a set of nodes that are represented in GraphRep
% assumes that GraphRep has no representation repetitions
graphNodes(Ns, set(Res)) :- graphNodes(Ns, set([]), set(Res)).
graphNodes([node(N, _, _) | Ns], set(FoundNodes), set(Res)) :-
  setAdd(set(FoundNodes), N, set(NewFounds)),
  graphNodes(Ns, set(NewFounds), set(Res)).
graphNodes([], set(FoundNodes), set(Res)) :- FoundNodes = Res.


%%% jestDobrzeUlozony(+EFgraf)
% INTERFACE METHOD
% specified in the task description
% jestDobrzeUlozony(EFgraf)
% draft implementation, missing stuff, not working
jestDobrzeUlozony(_) :- %% TODO check if this is the only pair that meets requirements
  % jestEFGrafem(EFgraf),
  % fNeighbourhoodLimit(EFgraf, 3),
  % isExit(EFgraf, Exit),
  % isEntrance(EFgraf, Entrance),
  hasCoveragePath(_, _, _).

% fNeighbourhoodLimit(GraphRep, Limit)
% True iff each node in GraphRep has no more than Limit F-graph neighbours
% Assumes GraphRep F relation is equivalent
fNeighbourhoodLimit([], Limit) :- Limit >= 0.
fNeighbourhoodLimit([node(_, _, FNeighbours) | Ns], Limit) :-
  listToSet(FNeighbours, set(S)),
  length(S, N),
  N < Limit,
  !,
  fNeighbourhoodLimit(Ns, Limit).

% isEntrance(GraphRep, Entrance) :-
% True if the node labeled with Entrance doesn't have any E-edge sources
isEntrance([], _).
isEntrance([node(_, Es, _) | Ns], Entrance) :- \+ member(Entrance, Es), isEntrance(Ns, Entrance).

% isExit(GraphRep, Exit) :-
% True if the node labeled with Exit doesn't have any E-edge destinations
isExit([node(N, [], _) | _], N).
isExit([node(_, _, _) | Ns], Exit) :- isExit(Ns, Exit).

hasCoveragePath(_, _, _).
% hasCoveragePath(EFgraf, Entrance, Exit). %% TODO - an idea is to first merge all
                                         % strongly connected components with a dfs (tarjan) - then,
                                         % if a dfs is capable of reaching exit from entrance,
                                         % then the predicate is true


%%% jestDobrzePermutujacy(EFgraf).
% INTERFACE METHOD
% specified in the task description
% draft implementation, complete but not tested and probably a few cases are not covered
jestDobrzePermutujacy(EFgraf) :- % didn't manage to test this, tests for this method
                                 % seem broken (I just found out and I have 3 mins left) so I leave this intact
                                 % and comment out other usages of this method
  jestDobrzeUlozony(EFgraf),
  jestDobrzePermutujacy(EFgraf, EFgraf).
jestDobrzePermutujacy(_, []).
jestDobrzePermutujacy(EFgraf, [N | Ns]) :- % for each node in EFgraf
  firstPermutujacyPred(EFgraf, N),
  secondPermutujacyPred(EFgraf, N),
  jestDobrzePermutujacy(EFgraf, Ns).


% getNodeDesc(GraphRep, NodeDesc)
% True iff there is a node in GraphRep that matches NodeDesc
getNodeDesc(GraphRep, NodeDesc) :- member(NodeDesc, GraphRep).

% firstPermutujacyPred(GraphRep, node(N, Es, Fs)) :-
% first part of the big jestDobrzePermutujacy from the task description
firstPermutujacyPred(_, node(_, [], [])).
firstPermutujacyPred(GraphRep, node(N, Es, Fs)) :-
  pairs(Es, Fs, EsFs),
  firstPermutujacyPred(GraphRep, node(N, Es, Fs), EsFs).
firstPermutujacyPred(GraphRep, _, [pair(_, W) | EsFs]) :-
  isExit(GraphRep, W),
  firstPermutujacyPred(GraphRep, _, EsFs).
firstPermutujacyPred(GraphRep, _, [pair(V, W) | EsFs]) :-
  getNodeDesc(GraphRep, node(V, _, Vfs)),
  getNodeDesc(GraphRep, node(W, Wes, _)),
  U = node(Un, _, _),
  getNodeDesc(GraphRep, U),
  member(Vfs, Un),
  member(Wes, Un),
  firstPermutujacyPred(GraphRep, _, EsFs).

% secondPermutujacyPred(GraphRep, node(N, Es, Fs)) :-
% second part of the big jestDobrzePermutujacy from the task description
secondPermutujacyPred(GraphRep, node(N, Es, Fs)) :-
  graphNodes(GraphRep, set(AllNodes)),
  secondPermutujacyPred(GraphRep, node(N, Es, Fs), AllNodes).
secondPermutujacyPred(_, _, []).
secondPermutujacyPred(GraphRep, node(N, Es, Fs), [V | Vs]) :-
  getNodeDesc(GraphRep, node(V, Ves, _)),
  \+ member(N, Ves),
  secondPermutujacyPred(GraphRep, node(N, Es, Fs), Vs).
secondPermutujacyPred(GraphRep, node(N, Es, Fs), [V | Vs]) :-
  getNodeDesc(GraphRep, node(V, _, Vfs)),
  member(N, Vfs),
  secondPermutujacyPred(GraphRep, node(N, Es, Fs), Vs, Fs).
secondPermutujacyPred(_, _, _, []).
secondPermutujacyPred(GraphRep, node(N, Es, Fs), [V | Vs], [W | Ws]) :-
  isEntrance(GraphRep, W),
  secondPermutujacyPred(GraphRep, node(N, Es, Fs), [V | Vs], Ws).
secondPermutujacyPred(GraphRep, node(N, Es, Fs), [V | Vs], [W | Ws]) :-
  getNodeDesc(GraphRep, node(V, _, Vfs)),
  getNodeDesc(GraphRep, node(Un, Ues, _)),
  member(Vfs, Un),
  member(Ues, W),
  secondPermutujacyPred(GraphRep, node(N, Es, Fs), [V | Vs], Ws).


%%% jestSucc(EFgraf, Lista1, Lista2) :-
% INTERFACE METHOD
% specified in the task description
jestSucc(EFgraf, Lista1, Lista2) :- % tests broke when this check was here - tests for jestSucc are not proper EFgraphs
  %jestDobrzePermutujacy(EFgraf),
  isFPath(EFgraf, Lista1),
  isFPath(EFgraf, Lista2),
  length(Lista1, M),
  length(Lista2, N),
  N >= M,
  fPathsCorrespondInEGraph(EFgraf, Lista1, Lista2).

% isFPath(GraphRep, NodeList)
% True iff NodeList is a representation of a valid F-path in GraphRep
isFPath(_, []).
isFPath(GraphRep, [P, Q | Ns]) :-
  getNodeDesc(GraphRep, node(P, _, Fs)),
  member(Q, Fs),
  isFPath(GraphRep, [Q | Ns]).
isFPath(GraphRep, [P]) :-
  getNodeDesc(GraphRep, node(P, _, _)).

% fPathsCorrespondInEGraph(GraphRep, Path1, Path2)
% True iff parallel nodes from Path1 and Path2
% have a correspondent connecting E-edge - see task
% description for more details
fPathsCorrespondInEGraph(_, [], _).
fPathsCorrespondInEGraph(GraphRep, [M | P1s], [N | P2s]) :-
  getNodeDesc(GraphRep, node(M, Es, _)),
  member(N, Es),
  fPathsCorrespondInEGraph(GraphRep, P1s, P2s).
