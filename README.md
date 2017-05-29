# jats2tex
**jats2tex** converte JATS-XML para TeX.

## Formato dos templates
Em aberto, os templates usados pelo Jats2tex terão um formato de fácil escrita por
humanos e computadores, um mapa de chaves e valores com suporte a nesting
(por exemplo, `conf`, `yml`, `json`, `ini`).

## Implementação
A partir do formato do template com suporte a customização da renderização de
elementos e atributos em contextos diferentes, um tipo intermediário e um
renderizador estilo "Visitor", o programa lerá e executará um parser XML no
input, conseguindo um tipo 'Artigo' - ou falhando com entrada inválida.

O programa usa o template para configurar um renderizador desse tipo para
LaTeX, usando uma linguagem monádica exposta pelo pacote `HaTeX`.

## Tecnologia
A tecnologia usada para elaborar a solução será a linguagem de programação
Haskell e pacotes embutidos para:

- A construção de parsers
- Parsing de arquivos XML
- Renderização de LaTeX/ConText válido

## Metodologia
O trabalho será feito usando a metodologia Agile de desenvolvimento de
Software. Assim o trabalho será dividido em metas curtas (Sprints) com o
período de uma semana.

O projeto será disponibilizado online via GitHub, escrito usando código
aberto. Ao final de cada semana, uma versão será empacotada e publicada com as
melhorias executadas.

## Interfaces de Uso
### Web
Um endpoint `POST` receberá dados em formato JATS-XML e dará o texto convertido
para LaTeX como resposta. Opcionalmente, recebe também o arquivo/texto de um
template.

### CLI
Opções serão expostas pela linha de comando usando o `optparse-applicative`, o
comando recebe um template e uma entrada JATS-XML e escreve o resultado para a
saída padrão.
