
-- Definição das entidades
data Curso = Curso {
  codigo :: Int,
  nome :: String,
  quantidadePeriodos :: Int
} deriving (Show, Eq)

data Disciplina = Disciplina {
  codigoDisciplina :: Int,
  codigoCurso :: Int,
  nomeDisciplina :: String,
  periodo :: Int
} deriving (Show, Eq)

data Aluno = Aluno {
  matricula :: Int,
  nomeAluno :: String,
  codigoCursoAluno :: Int,
  periodoAluno :: Int
} deriving (Show, Eq)

data Nota = Nota {
  matriculaAluno :: Int,
  codigoDisciplinaNota :: Int,
  nota1 :: Float,
  nota2 :: Float
} deriving (Show, Eq)

-- Funções de cadastro
cadastrarCurso :: [Curso] -> Curso -> [Curso]
cadastrarCurso cursos curso = if any (\c -> codigo c == codigo curso) cursos
  then cursos -- curso já existe
  else curso : cursos

cadastrarDisciplina :: [Curso] -> [Disciplina] -> Disciplina -> [Disciplina]
cadastrarDisciplina cursos disciplinas disciplina = if any (\c -> codigo c == codigoCurso disciplina) cursos &&
                                                      not (any (\d -> codigoDisciplina d == codigoDisciplina disciplina) disciplinas)
  then disciplina : disciplinas -- disciplina pode ser cadastrada
  else disciplinas -- disciplina não pode ser cadastrada

cadastrarAluno :: [Curso] -> [Aluno] -> Aluno -> [Aluno]
cadastrarAluno cursos alunos aluno = if any (\c -> codigo c == codigoCursoAluno aluno) cursos &&
                                        not (any (\a -> matricula a == matricula aluno) alunos)
  then aluno : alunos -- aluno pode ser cadastrado
  else alunos -- aluno não pode ser cadastrado

cadastrarNota :: [Aluno] -> [Disciplina] -> [Nota] -> Nota -> [Nota]
cadastrarNota alunos disciplinas notas nota = if any (\a -> matricula a == matriculaAluno nota) alunos &&
                                                  any (\d -> codigoDisciplina d == codigoDisciplinaNota nota) disciplinas &&
                                                  not (any (\n -> matriculaAluno n == matriculaAluno nota &&
                                                                 codigoDisciplinaNota n == codigoDisciplinaNota nota) notas)
  then nota : notas -- nota pode ser cadastrada
  else notas -- nota não pode ser cadastrada

-- Funções de busca
buscarCursos :: [Curso] -> IO ()
buscarCursos cursos = do
  putStrLn "Lista de cursos cadastrados:"
  mapM_ print cursos

buscarAlunosPorCurso :: [Aluno] -> Int -> IO ()
buscarAlunosPorCurso alunos codigoCurso = do
  let alunosCurso = filter (\a -> codigoCursoAluno a == codigoCurso) alunos
  if null alunosCurso
    then putStrLn "Não há alunos cadastrados nesse curso."
    else do
      putStrLn "Lista de alunos do curso:"
      mapM_ print alunosCurso


buscarAlunosPorPeriodo :: [Aluno] -> Int -> IO ()
buscarAlunosPorPeriodo alunos periodo = do
  let alunosPeriodo = filter (\a -> periodoAluno a == periodo) alunos
  if null alunosPeriodo
    then putStrLn "Não há alunos cadastrados nesse período."
    else do
      putStrLn "Lista de alunos do período:"
      mapM_ print alunosPeriodo

buscarDisciplinasPorCurso :: [Disciplina] -> Int -> IO ()
buscarDisciplinasPorCurso disciplinas codigoCursoBusca = do
  let disciplinasCurso = filter (\d -> codigoCurso d == codigoCursoBusca) disciplinas
  if null disciplinasCurso
    then putStrLn "Não há disciplinas cadastradas nesse curso."
    else do
      putStrLn "Lista de disciplinas do curso:"
      mapM_ print disciplinasCurso


buscarDisciplinasPorPeriodo :: [Disciplina] -> Int -> IO ()
buscarDisciplinasPorPeriodo disciplinas periodoo = do
  let disciplinasPeriodo = filter (\d -> periodo d == periodoo) disciplinas
  if null disciplinasPeriodo
    then putStrLn "Não há disciplinas cadastradas nesse período."
    else do
      putStrLn "Lista de disciplinas do período:"
      mapM_ print disciplinasPeriodo


buscarNotasPorAluno :: [Nota] -> Int -> IO ()
buscarNotasPorAluno notas matricula = do
  let notasAluno = filter (\n -> matriculaAluno n == matricula) notas
  if null notasAluno
    then putStrLn "Não há notas cadastradas para esse aluno."
    else do
      putStrLn "Lista de notas do aluno:"
      mapM_ print notasAluno

main :: IO ()
main = do
    let cursos = []
    let disciplinas = []
    let alunos = []
    let notas = []
    menu cursos disciplinas alunos notas

menu :: [Curso] -> [Disciplina] -> [Aluno] -> [Nota] -> IO ()
menu cursos disciplinas alunos notas = do
  putStrLn "\nEscolha uma das opções abaixo:"
  putStrLn "1 - Buscar cursos cadastrados"
  putStrLn "2 - Buscar alunos por curso"
  putStrLn "3 - Buscar alunos por período"
  putStrLn "4 - Buscar disciplinas por curso"
  putStrLn "5 - Buscar disciplinas por período"
  putStrLn "6 - Buscar notas por aluno"
  putStrLn "7 - Cadastrar curso"
  putStrLn "8 - Cadastrar disciplina"
  putStrLn "9 - Cadastrar aluno"
  putStrLn "10 - Cadastrar nota"
  putStrLn "11 - Sair"

  opcaoStr <- getLine
  let opcao = read opcaoStr :: Int
  case opcao of
    1 -> do
        buscarCursos cursos
        menu cursos disciplinas alunos notas
    2 -> do
        putStrLn "Digite o código do curso:"
        codigoCursoStr <- getLine
        let codigoCurso = read codigoCursoStr :: Int
        buscarAlunosPorCurso alunos codigoCurso
        menu cursos disciplinas alunos notas
    3 -> do
        putStrLn "Digite o período:"
        periodoStr <- getLine
        let periodo = read periodoStr :: Int
        buscarAlunosPorPeriodo alunos periodo
        menu cursos disciplinas alunos notas
    4 -> do
        putStrLn "Digite o código do curso:"
        codigoCursoStr <- getLine
        let codigoCurso = read codigoCursoStr :: Int
        buscarDisciplinasPorCurso disciplinas codigoCurso
        menu cursos disciplinas alunos notas
    5 -> do
        putStrLn "Digite o período:"
        periodoStr <- getLine
        let periodo = read periodoStr :: Int
        buscarDisciplinasPorPeriodo disciplinas periodo
        menu cursos disciplinas alunos notas
    6 -> do
        putStrLn "Digite a matrícula do aluno:"
        matriculaStr <- getLine
        let matricula = read matriculaStr :: Int
        buscarNotasPorAluno notas matricula
        menu cursos disciplinas alunos notas
    7 -> do
        putStrLn "Digite o código do curso:"
        codigoCursoStr <- getLine
        let codigoCurso = read codigoCursoStr :: Int
        putStrLn "Digite o nome do curso:"
        nomeCurso <- getLine
        putStrLn "Digite a quantidade de períodos:"
        quantidadePeriodosStr <- getLine
        let quantidadePeriodos = read quantidadePeriodosStr :: Int
        let novoCurso = Curso codigoCurso nomeCurso quantidadePeriodos
        let cursosAtualizados = cadastrarCurso cursos novoCurso
        putStrLn "Curso cadastrado com sucesso!"
        menu cursosAtualizados disciplinas alunos notas
    8 -> do
        putStrLn "Digite o código da disciplina:"
        codigoDisciplinaStr <- getLine
        let codigoDisciplina = read codigoDisciplinaStr :: Int
        putStrLn "Digite o código do curso:"
        codigoCursoStr <- getLine
        let codigoCurso = read codigoCursoStr
        putStrLn "Digite o nome da disciplina:"
        nomeDisciplina <- getLine
        putStrLn "Digite o período:"
        periodoStr <- getLine
        let periodo = read periodoStr :: Int
        let novaDisciplina = Disciplina codigoDisciplina codigoCurso nomeDisciplina periodo
        let disciplinasAtualizadas = cadastrarDisciplina cursos disciplinas novaDisciplina
        putStrLn "Disciplina cadastrada com sucesso!"
        menu cursos disciplinasAtualizadas alunos notas
    9 -> do
        putStrLn "Digite a matrícula do aluno:"
        matriculaStr <- getLine
        let matricula = read matriculaStr :: Int
        putStrLn "Digite o nome do aluno:"
        nomeAluno <- getLine
        putStrLn "Digite o código do curso do aluno:"
        codigoCursoAlunoStr <- getLine
        let codigoCursoAluno = read codigoCursoAlunoStr :: Int
        putStrLn "Digite o periodo do aluno:"
        periodoAlunoStr <- getLine
        let periodoAluno = read periodoAlunoStr :: Int
        let novoAluno = Aluno matricula nomeAluno codigoCursoAluno periodoAluno
        let alunosAtualizados = cadastrarAluno cursos alunos novoAluno
        putStrLn "Aluno cadastrado com sucesso!"
        menu cursos disciplinas alunosAtualizados notas

    10 -> do
        putStrLn "Digite a matrícula do aluno:"
        matriculaAlunoStr <- getLine
        let matriculaAluno = read matriculaAlunoStr :: Int
        putStrLn "Digite o código da disciplina:"
        codigoDisciplinaNotaStr <- getLine
        let codigoDisciplinaNota = read codigoDisciplinaNotaStr :: Int
        putStrLn "Digite a nota 1:"
        nota1Str <- getLine
        let nota1 = read nota1Str :: Float
        putStrLn "Digite a nota 2:"
        nota2Str <- getLine
        let nota2 = if null nota2Str then -1 else read nota2Str :: Float
        let novaNota = Nota matriculaAluno codigoDisciplinaNota nota1 nota2
        let notasAtualizadas = cadastrarNota alunos disciplinas notas novaNota
        putStrLn "Nota cadastrada com sucesso!"
        menu cursos disciplinas alunos notasAtualizadas


