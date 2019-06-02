# typeflow

A language where casting is the only operation

## Example

The following should work as a program:

```typeflow
# Define m (money) to be an integer.
# This means m can implicitly become an integer and integers can explicitly become m.
m i,
# Integers can be explicitly turned into banks. The bank can be implicitly turned into m.
bank m(i),
# Make a bank from the integer 5.
bank(5),
# Integers become deposit explicitly. deposit becomes m implicitly.
deposit m(i),
# A bank can be made explicitly from bank and deposit.
# Because the most recent definition is always used first, this will get used instead of
# the definition bank m(i).
bank bank(bank + deposit),
# Create a deposit from the most recent bank, which was bank(5), and a deposit_money(4).
bank(deposit(4), bank),
# Prints the bank. This will actually print "bank(deposit(m(4)), bank(m(5)))". This is because
# typeflow evaluates lazily, so nothing has been computed yet.
print(bank)
# Evaluate then print the bank. In this case bank -> m -> i.
# This works becase eval attempts to convert its entire environment to primitives in this order:
# u (unsigned), i (integer), f (float), s (string)
print(eval(bank))
```
