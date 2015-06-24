import turtle
from shapes import square, trapezoid, rectangle


def house(t, scale):
    rectangle(t, 300, 200)  # main body
    rectangle(t, 100, 150, 100, 0)  # door
    trapezoid(t, 400, -50, 200)  # roof

    # left window
    square(t, 50, 25, 80)
    rectangle(t, 0, 50, 50, 80)
    rectangle(t, 50, 0, 25, 105)

    # right window
    square(t, 50, 225, 80)
    rectangle(t, 0, 50, 250, 80)
    rectangle(t, 50, 0, 225, 105)


if __name__ == '__main__':
    wn = turtle.Screen()
    bob = turtle.Turtle()
    bob.speed(0)

    # pre-position
    bob.penup()
    bob.backward(100)
    bob.left(90)
    bob.backward(100)
    bob.right(90)
    bob.pendown()

    house(bob, 1)
    house(bob, 2)
