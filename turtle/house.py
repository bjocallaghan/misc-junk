import turtle
import shapes
import random


class BuilderTurtle(shapes.ShapeTurtle):
    def house(self, scale):
        self.rectangle(300*scale, 200*scale)  # main body
        self.rectangle(100*scale, 150*scale, 100*scale, 0*scale)  # door
        self.trapezoid(400*scale, -50*scale, 200*scale)  # roof
        self.window(25, 80, scale)  # left window
        self.window(225, 80, scale)  # right window

    def factory(self, scale):
        self.rectangle(300*scale, 200*scale)
        self.trapezoid(150*scale, 300*scale, 150*scale, 90)
        self.trapezoid(150*scale, 250*scale, 150*scale, 90)
        self.window(25, 80, scale)  # left window
        self.window(125, 80, scale)  # right window
        self.window(225, 80, scale)  # right window

    def window(self, x, y, scale):
        self.square(50*scale, x*scale, y*scale)
        self.rectangle(0*scale, 50*scale, (x+25)*scale, y*scale)
        self.rectangle(50*scale, 0*scale, x*scale, (y+25)*scale)


if __name__ == '__main__':
    wn = turtle.Screen()
    bob = BuilderTurtle()
    bob.speed(0)
    bob.penup()

    # pre-position
    bob.backward(300)
    bob.left(90)
    bob.forward(200)
    bob.right(90)

    for _ in range(4):
        row_length = 0
        for _ in range(7):
            scale = .05 * random.randrange(3, 6)
            bob.forward(50*scale)
            row_length += (50*scale)
            bob.pendown()
            bob.color(random.choice(["green", "blue", "orange", "purple"]))
            random.choice([bob.house, bob.factory])(scale)
            bob.penup()
            bob.forward(400*scale)
            row_length += (400*scale)
        bob.backward(row_length)
        bob.right(90)
        bob.forward(120)
        bob.left(90)
