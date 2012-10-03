(*1, define types for levels*)
type level = Leader | Expert | Establisher | Starter | Common


(*2*)
let getLevel n = 
	if n >= 400 then Leader
		else if n >= 150 then Expert
			else if n >= 60 then Establisher
				else if n >= 12 then Starter
					else Common

(*3*)
let getDiscount l = 
	match l with Leader -> 0.6
	| Expert -> 0.4
	| Establisher -> 0.2
	| Starter -> 0.05
	| Common -> 0.0

(*4*)
(*
1, LeafComm is the leaf nodes, denote the common level customers
2, NodeEnthusiastic is the intermidiate nodes in the tree, denote the other four level customers
3, the list of other customers contained in a enthusiastic customer is contained with type managementTree list.
4, RootNode is the root node, the entry to the whole tree.
5, the subtree of a node contains all customers invited by the node, or customers invited 
		by customers of the node.
6, if a customer is not invited by any other customers, it is directly linked to RootNode.

*)
type managementTree = 
	LeafComm of (string*int)
	| NodeEnthusiastic of (string*int*level*managementTree list)
	| RootNode of managementTree list


(*5*)
(* update one record to the tree
	We apply List.map to apply function updateOneSale (name,newnum) to each subtree list,
	in order to renew the information.
	Notice that the algorithm applied below traverse the entire tree for every new record, which is very inefficient.
*)
let rec updateOneSale (name,newnum) mTree = 
	match mTree with LeafComm(customer,oldnum) -> 
		if customer = name then LeafComm (customer,newnum) else mTree
	| NodeEnthusiastic(customer,oldnum,lev,mList) -> 
		if customer = name then 
			NodeEnthusiastic(customer,newnum,lev,mList)
		else 
			NodeEnthusiastic( customer,oldnum,lev,(List.map (updateOneSale (name,newnum)) mList) )
	| RootNode(mList) -> RootNode (List.map (updateOneSale (name,newnum)) mList)

(* update the new can bought information list l to the managementTree *)
let rec updateSales l mTree = 
	match l with [] -> mTree
	| head::tail -> updateSales tail (updateOneSale head mTree)


(*6*)
(*the following function is for calculating the total sells of one customer*)
let rec getSells mTree =
	match mTree with LeafComm(customer,num) -> num
	| NodeEnthusiastic(customer,num,lev,mList) -> 
		num + (List.fold_right (+) (List.map getSells mList) 0)
	| RootNode(mList) -> 
		List.fold_right (+) (List.map getSells mList) 0

let rec updateLevels mTree = 
	match mTree with LeafComm(customer,num) -> 
		if getLevel num != Common then NodeEnthusiastic(customer,num,getLevel num,[])
		else mTree
	| NodeEnthusiastic(customer,num,lev,mList) -> 
		let newLevel = getLevel (getSells mTree) in
			NodeEnthusiastic(customer,num,newLevel,List.map updateLevels mList)
	| RootNode(mList) -> 
		RootNode(List.map updateLevels mList)



