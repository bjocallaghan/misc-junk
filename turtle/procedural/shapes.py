import turtle
import math


def shape_pregame(t, offset_x, offset_y, rot_angle, color=None):
    old_color = t.color()[0]
    if not color:
        color = old_color
    old_speed = t.speed()
    t.speed(0)
    print color
    t.color(color)
    t.penup()
    t.forward(offset_x)
    t.left(90)
    t.forward(offset_y)
    t.right(90)
    t.left(rot_angle)
    t.pendown()
    t.speed(old_speed)
    return old_speed, old_color


def shape_postgame(t, offset_x, offset_y, rot_angle, old_speed, old_color):
    t.speed(0)
    t.penup()
    t.right(rot_angle)
    t.left(90)
    t.backward(offset_y)
    t.right(90)
    t.backward(offset_x)
    t.pendown()
    t.speed(old_speed)
    t.color(old_color)


def square(t, side_length, offset_x=0, offset_y=0, rot_angle=0, color=None):
    if not t.isdown():
        return
    speed, old_color = shape_pregame(t, offset_x, offset_y, rot_angle, color)
    for _ in range(4):
        t.forward(side_length)
        t.left(90)
    shape_postgame(t, offset_x, offset_y, rot_angle, speed, old_color)


def rectangle(t, width, height, offset_x=0, offset_y=0, rot_angle=0, color=None):
    if not t.isdown():
        return
    speed, old_color = shape_pregame(t, offset_x, offset_y, rot_angle, color)
    for _ in range(2):
        t.forward(width)
        t.left(90)
        t.forward(height)
        t.left(90)
    shape_postgame(t, offset_x, offset_y, rot_angle, speed, old_color)

def trapezoid(t, longest_length, offset_x=0, offset_y=0, rot_angle=0, color=None):
    if not t.isdown():
        return
    speed, old_color = shape_pregame(t, offset_x, offset_y, rot_angle, color)
    x = longest_length / 3
    t.forward(x * 3)
    t.left(135)
    t.forward(x * math.sqrt(2))
    t.left(45)
    t.forward(x)
    t.left(45)
    t.forward(x * math.sqrt(2))
    t.left(135)
    shape_postgame(t, offset_x, offset_y, rot_angle, speed, old_color)

if __name__ == '__main__':
    wn = turtle.Screen()
    bob = turtle.Turtle()

    square(bob, 60, color="red")
    square(bob, 60, color="blue")

    square(bob, 70, 100, 200, color="red")
    square(bob, 70, 100, 200, color="blue")

    square(bob, 70, -100, 200, 45, color="red")
    square(bob, 70, -100, 200, 45, color="blue")

    trapezoid(bob, 60, color="red")
    trapezoid(bob, 60, color="blue")

    trapezoid(bob, 70, 100, 200, color="red")
    trapezoid(bob, 70, 100, 200, color="blue")

    trapezoid(bob, 70, -100, 200, 45, color="red")
    trapezoid(bob, 70, -100, 200, 45, color="blue")

    rectangle(bob, 60, 20, color="red")
    rectangle(bob, 60, 20, color="blue")

    rectangle(bob, 70, 100, 100, 200, color="red")
    rectangle(bob, 70, 100, 100, 200, color="blue")

    rectangle(bob, 30, 80, -100, 200, 45, color="red")
    rectangle(bob, 30, 80, -100, 200, 45, color="blue")
