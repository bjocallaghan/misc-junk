import turtle
import math


class ShapeTurtle(turtle.Turtle):
    def shape_pregame(self, x, y, rot, color=None):
        old_color = self.color()[0]
        if not color:
            color = old_color
        old_speed = self.speed()
        self.speed(0)
        self.color(color)
        self.penup()
        self.forward(x)
        self.left(90)
        self.forward(y)
        self.right(90)
        self.left(rot)
        self.pendown()
        self.speed(old_speed)
        return old_speed, old_color

    def shape_postgame(self, x, y, rot, old_speed, old_color):
        self.speed(0)
        self.penup()
        self.right(rot)
        self.left(90)
        self.backward(y)
        self.right(90)
        self.backward(x)
        self.pendown()
        self.speed(old_speed)
        self.color(old_color)

    def square(self, side_length, x=0, y=0, rot=0, color=None):
        if not self.isdown():
            return
        speed, old_color = self.shape_pregame(x, y, rot, color)
        for _ in range(4):
            self.forward(side_length)
            self.left(90)
        self.shape_postgame(x, y, rot, speed, old_color)

    def rectangle(self, width, height, x=0, y=0, rot=0, color=None):
        if not self.isdown():
            return
        speed, old_color = self.shape_pregame(x, y, rot, color)
        for _ in range(2):
            self.forward(width)
            self.left(90)
            self.forward(height)
            self.left(90)
        self.shape_postgame(x, y, rot, speed, old_color)

    def trapezoid(self, longest_length, x=0, y=0, rot=0, color=None):
        if not self.isdown():
            return
        speed, old_color = self.shape_pregame(x, y, rot, color)
        a = longest_length / 3
        self.forward(a * 3)
        self.left(135)
        self.forward(a * math.sqrt(2))
        self.left(45)
        self.forward(a)
        self.left(45)
        self.forward(a * math.sqrt(2))
        self.left(135)
        self.shape_postgame(x, y, rot, speed, old_color)


if __name__ == '__main__':
    wn = turtle.Screen()
    bob = ShapeTurtle()

    bob.square(60, color="red")
    bob.square(60, color="blue")

    bob.square(70, 100, 200, color="red")
    bob.square(70, 100, 200, color="blue")

    bob.square(70, -100, 200, 45, color="red")
    bob.square(70, -100, 200, 45, color="blue")

    bob.trapezoid(60, color="red")
    bob.trapezoid(60, color="blue")

    bob.trapezoid(70, 100, 200, color="red")
    bob.trapezoid(70, 100, 200, color="blue")

    bob.trapezoid(70, -100, 200, 45, color="red")
    bob.trapezoid(70, -100, 200, 45, color="blue")

    bob.rectangle(60, 20, color="red")
    bob.rectangle(60, 20, color="blue")

    bob.rectangle(70, 100, 100, 200, color="red")
    bob.rectangle(70, 100, 100, 200, color="blue")

    bob.rectangle(30, 80, -100, 200, 45, color="red")
    bob.rectangle(30, 80, -100, 200, 45, color="blue")
