{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{- | short aliases for the keys of a keyboard.

when writing keyboard shortcuts, watch out for shadowed bindings like:

* @press 'm' where :: 'Modifier'@
* @press 'k' where :: 'Key'@
* @press 'c' where :: Char@
* @press 's' where :: String@
* @press 'i' where :: Integer@

if you import this module, your single-character-identifier typos (e.g. from refactoring) become confusing: @"can't unify \'a\' with Key@ instead of a scope error @\'a\' not in scope@. use at your own risk.

-}
module Commands.Sugar.Alias where
import Commands.Backends.OSX.Types


met = Command
ctrl = Control
shift = Shift
alt = Option

del = DeleteKey
tab = TabKey
spc = SpaceKey
ret = ReturnKey
esc = EscapeKey

left = LeftArrowKey
right = RightArrowKey
down = DownArrowKey
up = UpArrowKey

f1 = F1Key
f2 = F2Key
f3 = F3Key
f4 = F4Key
f5 = F5Key
f6 = F6Key
f7 = F7Key
f8 = F8Key
f9 = F9Key
f10 = F10Key
f11 = F11Key
f12 = F12Key
f13 = F13Key
f14 = F14Key
f15 = F15Key
f16 = F16Key
f17 = F17Key
f18 = F18Key
f19 = F19Key
f20 = F20Key

a = AKey
b = BKey
c = CKey
d = DKey
e = EKey
f = FKey
g = GKey
h = HKey
i = IKey
j = JKey
k = KKey
l = LKey
m = MKey
n = NKey
o = OKey
p = PKey
q = QKey
r = RKey
s = SKey
t = TKey
u = UKey
v = VKey
w = WKey
x = XKey
y = YKey
z = ZKey