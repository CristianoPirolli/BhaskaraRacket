#lang racket/gui

(require math)

(define equacao-frame%
  (class frame%
    (super-new [label "🧮 Calculadora de Equação do 2º Grau"]
               [width 400] [height 300])

    ;; Fontes para melhor aparência
    (define fonte-titulo (make-object font% 14 'modern 'italic))
    (define fonte-normal (make-object font% 12 'modern))

    ;; Painel principal
    (define painel-principal
      (new vertical-panel%
           [parent this]
           [alignment '(center center)]
           [spacing 10]
           [border 10]))

    ;; Título
    (define rotulo-titulo
      (new message%
           [parent painel-principal]
           [label "Insira os coeficientes da equação ax² + bx + c = 0"]
           [font fonte-titulo]))

    ;; Função para criar um campo de entrada com rótulo
    (define (criar-campo label-text)
      (define painel (new horizontal-panel%
                          [parent painel-principal]
                          [alignment '(center center)]))
      (new message% [parent painel] [label label-text] [font fonte-normal])
      (new text-field% [parent painel] [label ""] [min-width 100]))

    ;; Campos de entrada para a, b, c
    (define campo-a (criar-campo "Coeficiente a: "))
    (define campo-b (criar-campo "Coeficiente b: "))
    (define campo-c (criar-campo "Coeficiente c: "))

    ;; Área de resultado
    (define resultado-editor (new text%))
    (define campo-resultado
      (new editor-canvas%
           [parent painel-principal]
           [editor resultado-editor]
           [min-height 80]
           [stretchable-height #t]))

    ;; Botão Calcular
    (define botao-calcular
      (new button%
           [parent painel-principal]
           [label "Calcular"]
           [callback
            (lambda (_btn _evt)
              (processar-valores))]))

    ;; Função para processar os coeficientes
    (define (processar-valores)
      (let* ([str-a (send campo-a get-value)]
             [str-b (send campo-b get-value)]
             [str-c (send campo-c get-value)]
             [num-a (string->number str-a)]
             [num-b (string->number str-b)]
             [num-c (string->number str-c)])
        (cond
          [(not (and num-a num-b num-c))
           (exibir-resultado "❌ Erro: Insira números válidos para a, b e c.")]
          [(zero? num-a)
           (exibir-resultado "⚠️ Erro: O coeficiente 'a' não pode ser zero.")]
          [else
           (calcular-e-mostrar num-a num-b num-c)])))

    ;; Função para exibir o resultado
    (define (exibir-resultado texto)
      (send resultado-editor erase)
      (send resultado-editor insert texto))

    ;; Função para cálculo e exibição do resultado
    (define (calcular-e-mostrar a b c)
      (let* ([delta (- (* b b) (* 4 a c))]
             [vx (/ (- b) (* 2 a))]
             [vy (/ (- delta) (* 4 a))])
        (if (< delta 0)
            (exibir-resultado
             (format "Delta = ~a\n❌ Não existem raízes reais.\n📍 Vértice: (~a, ~a)"
                     delta vx vy))
            (let* ([raiz (sqrt delta)]
                   [x1 (/ (+ (- b) raiz) (* 2 a))]
                   [x2 (/ (- (- b) raiz) (* 2 a))])
              (exibir-resultado
               (format "Delta = ~a\n✅ Raiz x1 = ~a\n✅ Raiz x2 = ~a\n📍 Vértice: (~a, ~a)"
                       delta x1 x2 vx vy))))))))

;; Cria e mostra a janela
(define minha-janela (new equacao-frame%))
(send minha-janela show #t)
