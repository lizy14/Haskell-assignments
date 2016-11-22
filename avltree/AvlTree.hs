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
	key = undefined

	left = undefined

	right = undefined

	height = undefined

	size = undefined

	collect = undefined

	contains = undefined

-- 成为 SortedBinaryTree 类型类的实例
instance SortedBinaryTree AvlTree where
	-- 把这些名字绑定到 undefined，使得在开始改动任何东西之前，本文件就能通过编译。
	-- 把这些 undefined 改成真正的函数实现，就像以前实现函数那样。
	search = undefined

	insert = undefined
	
	remove = undefined

-- AvlTree 独有的、不属于某个类型类的函数，定义在外面
-- 当然不必叫 rotateLL 这个名字……
rotateLL :: AvlTree a -> AvlTree a
rotateLL = undefined

-- 成为 Functor 类型类的实例
instance Functor AvlTree where
	fmap = undefined

-- 成为 Foldable 类型类的实例
instance Foldable AvlTree where
	foldr = undefined
	


