import Data.Foldable (Foldable(length)) 
type Libro = (String, String, Int)

elVisitante :: Libro
elVisitante = ("El visitante", "Stephen King", 592)

shingekiNoKyojin1 :: Libro
shingekiNoKyojin1 = ("Shingeki no Kyojin 1", "Hajime Isayama", 40)

shingekiNoKyojin2 :: Libro
shingekiNoKyojin2 = ("Shingeki no Kyojin 2", "Hajime Isayama", 40)

shingekiNoKyojin127 :: Libro
shingekiNoKyojin127 = ("Shingeki no Kyojin 127", "Hajime Isayama", 40)

fundacion :: Libro
fundacion = ("Fundación", "Isaac Asimov", 230)

sandman5 :: Libro
sandman5 = ("Sandman 5", "Neil Gaiman", 35)

sandman10 :: Libro
sandman10 = ("Sandman 10", "Neil Gaiman", 35)

sandman12 :: Libro
sandman12 = ("Sandman 12", "Neil Gaiman", 35)

type Saga = [Libro]
sagaDeEragon :: Saga
sagaDeEragon = [eragon,eldest,brisignr,legado]

eragon :: Libro
eragon = ("Eragon", "Christopher Paolini", 544)

eldest :: Libro
eldest = ("Eldest", "Christopher Paolini", 704)

brisignr :: Libro
brisignr = ("Brisignr", "Christopher Paolini", 700)

legado :: Libro
legado = ("Legado", "Christopher Paolini", 811)

type Biblioteca = [Libro]
biblioteca :: Biblioteca
biblioteca = [elVisitante, shingekiNoKyojin1, shingekiNoKyojin2, shingekiNoKyojin127, fundacion, sandman5, sandman10, sandman12, eragon, eldest, brisignr, legado]

promedioDePaginas :: Biblioteca -> Int
promedioDePaginas unaBiblioteca = div (cantidadDePaginasTotales unaBiblioteca) (length unaBiblioteca)

cantidadDePaginasTotales :: Biblioteca -> Int
cantidadDePaginasTotales unaBiblioteca = sum . map cantidadDePaginas $ unaBiblioteca

cantidadDePaginas :: Libro -> Int
cantidadDePaginas (_, _, paginas) = paginas


