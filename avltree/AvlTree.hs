module AvlTree where

import Data.Maybe

-- 二叉树是一类树。为此定义一个类型类
class BinaryTree t where
	-- 取得键、取得左子树、取得右子树。返回 Maybe a 而非 a，可以应对当前结点为 Nil 的情形。
	key :: t a -> Maybe a
	left :: t a -> Maybe (t a)
	right :: t a -> Maybe (t a)
	height :: t a -> Int
	size :: t a -> Int
	-- collect 返回中序遍历这棵树的结果。
	collect :: t a -> [a]
	contains :: Eq a => a -> t a -> Bool

-- 有序的二叉树也是一类树。它们首先要是二叉树。
class BinaryTree t => SortedBinaryTree t where
	search :: (Ord a) => a -> t a -> Maybe (t a)
	insert :: (Ord a) => a -> t a -> t a
	remove :: (Ord a) => a -> t a -> t a

-- Node k h l r 中，k 为该结点的键，h 为以该结点为根的子树的高度，l 和 r 为左右子树。
data AvlTree a = Nil | Node a Int (AvlTree a) (AvlTree a)
	deriving(Show, Read, Eq)

-- 用 leaf 函数可以在构造 AvlTree 时少打几个字
leaf :: a -> AvlTree a
leaf k = Node k 0 Nil Nil

-- 要成为 BinaryTree 类型类的实例，AvlTree 得实现这些函数
instance BinaryTree AvlTree where
	-- 把这些名字绑定到 undefined，使得在开始改动任何东西之前，本文件就能通过编译。
	key Nil = Nothing
	key (Node k _ _ _) = Just k

	left Nil = Nothing
	left (Node _ _ l _) = Just l

	right Nil = Nothing
	right (Node _ _ _ r) = Just r

	height Nil = 0
	height (Node _ h _ _) = h

	size Nil = 0
	size (Node _ _ l r) = size l + size r + 1

	collect Nil = []
	collect (Node k _ l r) = collect l ++ [k] ++ collect r

	contains key Nil = False
	contains key (Node k _ l r) = or [
			k == key,
			contains key l,
			contains key r
		]

-- 成为 SortedBinaryTree 类型类的实例
instance SortedBinaryTree AvlTree where
	-- 把这些名字绑定到 undefined，使得在开始改动任何东西之前，本文件就能通过编译。
	-- 把这些 undefined 改成真正的函数实现，就像以前实现函数那样。
	search _ Nil = Nothing
	search key node@(Node k _ l r)
		| key < k = search key l
		| key > k = search key r
		| otherwise = Just node

	insert v Nil = leaf v
	insert v node@(Node k h l r)
		| v < k = let l' = insert v l in balance $ makeParent k l' r
		| v > k = let r' = insert v r in balance $ makeParent k l r'
		| otherwise = node

	remove = undefined

-- AvlTree 独有的、不属于某个类型类的函数，定义在外面

makeParent :: a -> AvlTree a -> AvlTree a -> AvlTree a
makeParent k l r =
	Node k (makeHeight l r) l r
	where makeHeight l r = 1 + max (height l) (height r)

{-
    a        b
   / \      / \
  b   c    d   a'
 / \          / \
d   e        e   c
-}
rotateLL :: AvlTree a -> AvlTree a
rotateLL (Node ak ah (Node bk bh d e) c) =
	makeParent bk d a'
	where a' = makeParent ak e c

{-
  a          c
 / \        / \
b   c      a'  e
   / \    / \
  d   e  b   d
-}
rotateRR :: AvlTree a -> AvlTree a
rotateRR (Node ak ah b (Node ck ch d e)) =
	makeParent ck a' e
	where a' = makeParent ak b d

rotateLR :: AvlTree a -> AvlTree a
rotateLR (Node k h l r) = rotateLL (Node k h (rotateRR l) r)

rotateRL :: AvlTree a -> AvlTree a
rotateRL (Node k h l r) = rotateRR (Node k h l (rotateLL r))

--do nothing if balanced, rotation otherwise
balance :: AvlTree a -> AvlTree a
balance t@(Node k h l r)
	| (abs $ height l - height r) < 2 = t
	| otherwise = rotate t

rotate :: AvlTree a -> AvlTree a
rotate node@(Node k h l r) = (
	if leftHigher node then
		if leftHigher l then
			rotateLL
		else
			rotateLR
	else
		if leftHigher r then
			rotateRL
		else
			rotateRR
	) node
	where leftHigher (Node k h l r) = (height l) > (height r)




-- 成为 Functor 类型类的实例
instance Functor AvlTree where
	fmap _ Nil = Nil
	fmap f node@(Node k h l r) = Node
		(f k)
		h
		(fmap f l)
		(fmap f r)

-- 成为 Foldable 类型类的实例
instance Foldable AvlTree where
	foldr f z node = foldr f z $ collect node
