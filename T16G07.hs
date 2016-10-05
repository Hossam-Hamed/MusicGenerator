import Data.List
import MusicResources
x=chars
y=chars
n= (length chars) -1
---------------------------------Part A--------------------------------
getModule element 0  =[element++[y!!n]]
getModule element count   =(element++[y!!count]):getModule element (count-1) 

getModule1 element= getModule element n								
								
findMinY [] _ n=n
findMinY _ [] n=n
findMinY _ (l:[]) n=n
findMinY m (l:(r:t)) n = if (m!!0==l) && (m!!1==r) then  findMinY m(r:t) n+1 else  findMinY m(r:t) n

freq_count m  = findMinY m (concat training) 0

brackets list 0  =[(freq_count(list!!n),((list!!n)!!1))]
brackets list count = (freq_count(list!!count),((list!!count)!!1)):brackets list (count-1) 

brackets1 list = brackets list n

removeZeros::[(Int,Char)]->[(Int,Char)]
removeZeros []=[]
removeZeros (h:t) =if(fst(h)==0) then removeZeros t else h:removeZeros t

desc_sortedBrackets count g = reverse (sort (removeZeros (brackets1( g))))

makeStatsList1 0 =  [( x!!0, desc_sortedBrackets 0 (getModule1 [x!!0]) )]
makeStatsList1 count = ( x!!count, desc_sortedBrackets count (getModule1 [x!!count])):makeStatsList1(count-1)

makeStatsList::[(Char,[(Int,Char)])]
makeStatsList= reverse(makeStatsList1 n)

---------------------------------Part B--------------------------------

decompose (0,k)= []
decompose (h,k)= k:decompose(h-1,k)

getString []=[]
getString (h:t)= decompose h:getString t

getString1 w = concat (getString w)

get_list c count m = if(c==fst(entry)) then snd(entry)  else get_list c (count+1) m
	where entry = m!!count

get_list1 c m =get_list c 0 m

get_char0 s=if s == [] then error "Stuck .. no possible next character" else s!!randomZeroToX(length s-1)

get_char c m = get_char0 (getString1(get_list1 c m))


compose::Char-> Int->[Char]
compose char 1 = [get_char char makeStatsList]
compose char i = char : compose(get_char char makeStatsList) (i-1)



-----------------------------------------------------------------------------------------------------
