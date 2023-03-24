import System.IO
import Data.Csv

-- Definição das entidades
data Curso = Curso {
  codigo :: Int,
  nome :: String,
  quantidadePeriodos :: Int
} deriving (Show, Eq, Generic)

instance FromRecord Curso
instance ToRecord Curso

data Disciplina = Disciplina {
  codigoDisciplina :: Int,
  codigoCurso :: Int,
  nomeDisciplina :: String,
  periodo :: Int
} deriving (Show, Eq, Generic)

instance FromRecord Disciplina
instance ToRecord Disciplina

data Aluno = Aluno {
  matricula :: Int,
  nomeAluno :: String,
  codigoCursoAluno :: Int,
  periodoAluno :: Int
} deriving (Show, Eq, Generic)

instance FromRecord Aluno
instance ToRecord Aluno

data Nota = Nota {
  matriculaAluno :: Int,
  codigoDisciplinaNota :: Int,
  nota1 :: Float,
  nota2 :: Float
} deriving (Show, Eq, Generic)

instance FromRecord Nota
instance ToRecord Nota

-- Funções de cadastro
cadastrarCurso :: FilePath -> Curso -> IO ()
cadastrarCurso file curso = do
  cursos <- lerCSV file
  let cursos' = if any (\c -> codigo c == codigo curso) cursos
                   then cursos -- curso já existe
                   else curso : cursos
  escreverCSV file cursos'

cadastrarDisciplina :: FilePath -> FilePath -> Disciplina -> IO ()
cadastrarDisciplina cursosFile disciplinasFile disciplina = do
  cursos <- lerCSV cursosFile
  disciplinas <- lerCSV disciplinasFile
  let disciplinas' = if any (\c -> codigo c == codigoCurso disciplina) cursos &&
                        not (any (\d -> codigoDisciplina d == codigoDisciplina disciplina) disciplinas)
                       then disciplina : disciplinas -- disciplina pode ser cadastrada
                       else disciplinas -- disciplina não pode ser cadastrada
  escreverCSV disciplinasFile disciplinas'

cadastrarAluno :: FilePath -> FilePath -> Aluno -> IO ()
cadastrarAluno cursosFile alunosFile aluno = do
  cursos <- lerCSV cursosFile
  alunos <- lerCSV alunosFile
  let alunos' = if any (\c -> codigo c == codigoCursoAluno aluno) cursos &&
                   not (any (\a -> matricula a == matricula aluno) alunos)
                   then aluno : alunos -- aluno pode ser cadastrado
                   else alunos -- aluno não pode ser cadastrado
  escreverCSV alunosFile alunos'

cadastrarNota :: FilePath -> FilePath -> FilePath -> Nota -> IO ()
cadastrarNota cursosFile disciplinasFile notasFile nota = do
  cursos <- lerCSV cursosFile
  disciplinas <- lerCSV disciplinasFile
  alunos <- lerCSV alunosFile
  notas <- lerCSV notasFile
  let notas' = if any (\a -> matricula a == matriculaAluno nota) alunos &&
                   any (\d -> codigoDisciplina d == codigoDisciplinaNota nota) disciplinas &&
                   not (any (\n -> matriculaAluno n == matriculaAluno nota &&
                                      codigoDisciplinaNota n == codigoDisciplinaNota nota) notas)
                   then nota : notas -- nota pode ser cadastrada
                   else notas -- nota não pode ser cadastrada
  escreverCSV notasFile notas'

-- Funções de busca
buscarCursos :: FilePath -> IO ()
buscarCursos arquivo = do
  conteudo <- readFile arquivo
  let cursos = parseCursoCSV conteudo
  putStrLn "Lista de cursos cadastrados:"
  mapM_ print cursos

buscarAlunosPorCurso :: FilePath -> FilePath -> Int -> IO ()
buscarAlunosPorCurso arquivoAluno arquivoCurso codigoCurso = do
  conteudoAluno <- readFile arquivoAluno
  conteudoCurso <- readFile arquivoCurso
  let alunos = parseAlunoCSV conteudoAluno
      cursos = parseCursoCSV conteudoCurso
      alunosCurso = filter (\a -> codigoCursoAluno a == codigoCurso) alunos
  if null alunosCurso
    then putStrLn "Não há alunos cadastrados nesse curso."
    else do
      let curso = head $ filter (\c -> codigoCurso c == codigoCurso) cursos
      putStrLn $ "Lista de alunos do curso " ++ nomeCurso curso ++ ":"
      mapM_ print alunosCurso

buscarAlunosPorPeriodo :: FilePath -> IO ()
buscarAlunosPorPeriodo arquivo periodo = do
  conteudo <- readFile arquivo
  let alunos = parseAlunoCSV conteudo
      alunosPeriodo = filter (\a -> periodoAluno a == periodo) alunos
  if null alunosPeriodo
    then putStrLn "Não há alunos cadastrados nesse período."
    else do
      putStrLn "Lista de alunos do período:"
      mapM_ print alunosPeriodo

buscarDisciplinasPorCurso :: FilePath -> FilePath -> Int -> IO ()
buscarDisciplinasPorCurso arquivoDisciplina arquivoCurso codigoCursoBusca = do
  conteudoDisciplina <- readFile arquivoDisciplina
  conteudoCurso <- readFile arquivoCurso
  let disciplinas = parseDisciplinaCSV conteudoDisciplina
      cursos = parseCursoCSV conteudoCurso
      disciplinasCurso = filter (\d -> codigoCurso d == codigoCursoBusca) disciplinas
  if null disciplinasCurso
    then putStrLn "Não há disciplinas cadastradas nesse curso."
    else do
      let curso = head $ filter (\c -> codigoCurso c == codigoCursoBusca) cursos
      putStrLn $ "Lista de disciplinas do curso " ++ nomeCurso curso ++ ":"
      mapM_ print disciplinasCurso


buscarDisciplinasPorPeriodo :: FilePath -> Int -> IO ()
buscarDisciplinasPorPeriodo arquivo periodoo = do
  conteudo <- readFile arquivo
  let disciplinas = parseDisciplinaCSV conteudo
      disciplinasPeriodo = filter (\d -> periodo d == periodoo) disciplinas
  if null disciplinasPeriodo
    then putStrLn "Não há disciplinas cadastradas nesse período."
    else do
      putStrLn "Lista de disciplinas do período:"
      mapM_ print disciplinasPeriodo


buscarNotasPorAluno :: FilePath -> Int -> IO ()
buscarNotasPorAluno arquivo matricula = do
  conteudo <- readFile arquivo
  let notas = parseNotaCSV conteudo
      notasAluno = filter (\n -> matriculaAluno n == matricula) notas
  if null notasAluno
    then putStrLn "Não há notas cadastradas para esse aluno."
    else do
      putStrLn "Lista de notas do aluno:"
      mapM_ print notasAluno
