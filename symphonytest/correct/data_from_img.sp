variables:
  x[533][446]

constants:
  img[533][446] = Image("sample_img.png")

minimize:
  norm2square (x - img)
