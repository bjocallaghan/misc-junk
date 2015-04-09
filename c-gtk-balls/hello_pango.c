#include <pango/pangocairo.h>

int main (int argc, char *argv[]) {
  cairo_surface_t *surface =
    cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 340, 80);
  cairo_t *cr = cairo_create(surface);

  PangoFontDescription *font_description = pango_font_description_new();
  pango_font_description_set_family(font_description, "serif");
  pango_font_description_set_weight(font_description, PANGO_WEIGHT_BOLD);
  pango_font_description_set_absolute_size (font_description, 32 * PANGO_SCALE);

  PangoLayout *layout = pango_cairo_create_layout (cr);
  pango_layout_set_font_description (layout, font_description);
  pango_layout_set_text (layout, "Hello, Pango world!", -1);

  cairo_set_source_rgb (cr, 1.0, 0.0, 0.0);
  cairo_move_to(cr, 10.0, 50.0);

  pango_cairo_show_layout(cr, layout);

  g_object_unref(layout);
  pango_font_description_free(font_description);

  cairo_destroy(cr);
  cairo_surface_write_to_png(surface, "hello.png");
  cairo_surface_destroy(surface);
}

void write_to_surface(cairo_t *cr) {
}
