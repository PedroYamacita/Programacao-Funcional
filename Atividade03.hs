import System.IO (isEOF)

-- Definição de tipo para a Agenda
type Agenda = [(String, String)]

main :: IO ()
main = agendaLoop []

agendaLoop :: Agenda -> IO ()
agendaLoop agenda = do
    done <- isEOF
    if done
        then return ()
        else do
            input <- getLine
            let parts = words input
            case parts of
                ["adicionar", nome, telefone] -> do
                    let novaAgenda = (nome, telefone) : filter (\(n, _) -> n /= nome) agenda
                    agendaLoop novaAgenda

                ["buscar", nome] -> do
                    case lookup nome agenda of
                        Just tel -> putStrLn $ nome ++ " - " ++ tel
                        Nothing  -> putStrLn "Contato nao encontrado."
                    agendaLoop agenda

                ["listar"] -> do
                    mapM_ (\(n, t) -> putStrLn $ n ++ " - " ++ t) agenda
                    agendaLoop agenda

                ["remover", nome] -> do
                    if any (\(n, _) -> n == nome) agenda
                        then do
                            let agendaFiltrada = filter (\(n, _) -> n /= nome) agenda
                            putStrLn "Contato removido."
                            agendaLoop agendaFiltrada
                        else do
                            putStrLn "Contato nao encontrado."
                            agendaLoop agenda

                ["sair"] -> putStrLn "Encerrando."

                _ -> agendaLoop agenda