-module(b1).
-compile(export_all).
-record(co,{char, count=1}).
-record(table,{char, bitList=[]}).
-type tree():: fork() | leaf().
-record(fork, {left::tree(), right::tree(), chars::list(char()), weight::non_neg_integer()}).
-type fork() :: #fork{}.
-record(leaf, {char::char(), weight::non_neg_integer()}).
-type leaf() :: #leaf{}.
-type bit() :: 0 | 1.

%---Main Functions-------------------------------------------------------------------------------------------
check_it(StringA)->                               %Creates BitString from CharString
  List=string_to_char_list(StringA),              %Strips String into CharList
  ListSorted= sort(List, fun b1:compS/2),         %Sorts CharList as preparation for character count
  Dict=char_list_to_dict(ListSorted),             %Counts characters in CharList and creates "co"Record dictionary, by counting how many identical characters are nxt to each other.
  DictSorted=sort(Dict, fun b1:compT/2),          %Sorts dictionary according to charweight
  Leaves=createLeaves(DictSorted),                %Creates Leaves from dictionary (DictSorted)
  Tree=createHuffTree(Leaves),                    %Takes two TreeNodes(fork or leaf), combines them to a new treenode and inserts it SORTED into the remaining List
  EncodingList=makeTable(Tree),                   %Traverses Tree and returns the "BitPath"-List in a Tuple together with the corresponding character
  BitList=encodeString(EncodingList, StringA),    %Traverses String and transforms it into Bits according to Bitpath given in the Tuples of "EncodingList"
  {lists:concat(BitList), Tree}.                  %Returns the Bitlist striped to one String and the corresponding HuffmanTree

uncheck_it(BitString, Tree)->
  BitList= string_to_char_list(BitString),        %Transform String of Bits(1/0) to List of Bits
  decodeBits(Tree, BitList).                      %Traverses Tree according to BitList until leaf is found.
                                                  %Returns CharValue of Leaf and adds all up to one String
%---Help Functions--------------------------------------------------------------------------------------------

string_to_char_list(StringA)->
  String=string:to_lower(StringA),
  Length=string:len(String),
  Curr=1,
  stcl(String,Length,Curr).
  stcl(String, Length, Curr) when Curr=<Length -> [string:substr(String,Curr,1)|stcl(String, Length, Curr+1)];
    stcl(String, Length, Curr) -> [].

char_list_to_dict(List)->char_list_to_dict(List,1).
char_list_to_dict([H,H|[]],Occ)->[#co{char=H,count=(Occ+1)}];
char_list_to_dict([H,H2|[]],Occ)->[#co{char=H,count=Occ}]++[#co{char=H2,count=1}];
char_list_to_dict([H,H|T],Occ)->char_list_to_dict([H|T], Occ+1);
char_list_to_dict([H,H2|T],Occ)->[#co{char=H, count=Occ}] ++ char_list_to_dict([H2|T],1).

%inserts nr into sorted List, returns sorted List
compS(X,Y)-> case (X>Y) of true -> Y; _ -> X end.
compT(N=#fork{weight=C}, H=#fork{weight=C2}) -> case (C>C2) of true -> H; _ -> N end;
compT(N=#fork{weight=C}, H=#leaf{weight=C2}) -> case (C>C2) of true -> H; _ -> N end;
compT(N=#co{count=C}, H=#co{count=C2})       -> case (C>C2) of true -> H; _ -> N end.

%Insert with Function into
insertSoS(N,[],MyFunc)-> [N];
insertSoS(N,[H|T], MyFunc) -> case MyFunc(N,H)==H of true ->  [H|insertSoS(N,T,MyFunc)]; _ -> [N,H|T] end.
%sort List with insertSo
sort(List, MyFunc)->sort(List, [], MyFunc).
sort([H|T], SList, MyFunc) -> sort(T, insertSoS(H, SList, MyFunc),MyFunc);
sort([], T, MyFunc) -> T.
%-------------------------------------------------
createLeaves([#co{char=C, count=N}|T])->[#leaf{char=C, weight=N}|createLeaves(T)];
createLeaves([])->[].

%join two #co records
joinLeavesNForks(H=#leaf{char=C, weight=N}, H2=#leaf{char=C2, weight=N2})->
  N3=N+N2,C3=[C|C2], #fork{chars=C3, weight=N3, left=H, right=H2};
joinLeavesNForks(H=#fork{chars=C, weight=N, left=L, right=R}, H2=#fork{chars=C2, weight=N2, left=L2, right=R2})->
  N3=N+N2,C3=C++C2, #fork{chars=C3, weight=N3, left=H, right=H2};
joinLeavesNForks(H=#leaf{char=C, weight=N}, H2=#fork{chars=C2, weight=N2, left=L2, right=R2})->
  N3=N+N2,C3=C++C2, #fork{chars=C3, weight=N3, left=H, right=H2};
joinLeavesNForks(H=#fork{chars=C, weight=N, left=L, right=R}, H2=#leaf{char=C2, weight=N2})->
  N3=N+N2,C3=C++C2, #fork{chars=C3, weight=N3, left=H, right=H2}.

%Creates a HuffmanTree from a list of leaves
createHuffTree([H1,H2|T])-> createHuffTree(insertSoS(joinLeavesNForks(H1,H2), T, fun b1:compT/2));
createHuffTree([H|[]])->H.

%Takes Bits and a HuffmanTree to decode
decodeBits(#fork{} = Orig, List)-> decodeBits(Orig, List, Orig).
decodeBits(#fork{left=L, right=R}, ["0"|T], Orig)->decodeBits(L,T, Orig);
decodeBits(#fork{left=L, right=R}, ["1"|T], Orig)->decodeBits(R,T, Orig);
decodeBits(#leaf{char=C}, T, Orig)->C ++ decodeBits(Orig, T, Orig);
%decodeBits(#leaf{char=C}, T, Orig)->C ++ decodeBits(Orig, T,Orig);
decodeBits(_,[],_)->"".

%Takes a HuffmanTree and creates a List of #table Records with a character and a List of corresponding Bits
makeTable(Tree)->makeTable(Tree, []).
makeTable(#fork{left=L, right=R}, [])-> makeTable(L, [0]) ++ makeTable(R, [1]);
makeTable(#fork{left=L, right=R}, KeyList)-> makeTable(L, KeyList++[0]) ++ makeTable(R, KeyList++[1]);
makeTable(#leaf{char=C}, KeyList)->
  [#table{char=C, bitList=KeyList}].

%encodes a String according to HuffmanTree Table
encodeString(TreeTable, String)-> encodeStringList(TreeTable, string_to_char_list(String), TreeTable).
encodeStringList([HT|TableList], [HC|CharList], OriginalTableList) ->
  if HT#table.char==HC -> HT#table.bitList ++ encodeStringList(OriginalTableList,CharList,OriginalTableList);
    true -> encodeStringList(TableList, [HC|CharList], OriginalTableList)
  end;
encodeStringList(_,[],_)->[].
