#lang racket/gui
(require db)
(require math)

;; Conecta ao banco SQLite
(define db (sqlite3-connect #:database "bhaskara.db"))

(define equacao-frame%
  (class frame%
    (super-new [label "🧮 Calculadora de Equação do 2º Grau"]
               [width 400] [height 300])

    (define fonte-titulo (make-object font% 14 'modern 'italic))
    (define fonte-normal (make-object font% 12 'modern))

    (define painel-principal
      (new vertical-panel%
           [parent this]
           [alignment '(center center)]
           [spacing 10]
           [border 10]))

    (define rotulo-titulo
      (new message%
           [parent painel-principal]
           [label "Insira os coeficientes da equação ax² + bx + c = 0"]
           [font fonte-titulo]))

    (define (criar-campo label-text)
      (define painel (new horizontal-panel%
                          [parent painel-principal]
                          [alignment '(center center)]))
      (new message% [parent painel] [label label-text] [font fonte-normal])
      (new text-field% [parent painel] [label ""] [min-width 100]))

    (define campo-a (criar-campo "Coeficiente a: "))
    (define campo-b (criar-campo "Coeficiente b: "))
    (define campo-c (criar-campo "Coeficiente c: "))

    (define resultado-editor (new text%))
    (define campo-resultado
      (new editor-canvas%
           [parent painel-principal]
           [editor resultado-editor]
           [min-height 80]
           [stretchable-height #t]))

    (define botao-calcular
      (new button%
           [parent painel-principal]
           [label "Calcular"]
           [callback
            (lambda (_btn _evt)
              (processar-valores))]))

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

    (define (exibir-resultado texto)
      (send resultado-editor erase)
      (send resultado-editor insert texto))

    (define (salvar-no-banco a b c delta x1 x2 vx vy)
      (query-exec db
                  "INSERT INTO bhaskara (valorA, valorB, valorC, delta, x1, x2, verticeX, verticeY)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?);"
                  a b c delta x1 x2 vx vy))

    (define (calcular-e-mostrar a b c)
      (let* ([delta (- (* b b) (* 4 a c))]
             [vx (/ (- b) (* 2 a))]
             [vy (/ (- delta) (* 4 a))])
        (if (< delta 0)
            (begin
              (salvar-no-banco a b c delta sql-null sql-null vx vy)
              (exibir-resultado
               (format "Delta = ~a\n❌ Não existem raízes reais.\n📍 Vértice: (~a, ~a)"
                       delta vx vy)))
            (let* ([raiz (sqrt delta)]
                   [x1 (/ (+ (- b) raiz) (* 2 a))]
                   [x2 (/ (- (- b) raiz) (* 2 a))])
              (salvar-no-banco a b c delta x1 x2 vx vy)
              (exibir-resultado
               (format "Delta = ~a\n✅ Raiz x1 = ~a\n✅ Raiz x2 = ~a\n📍 Vértice: (~a, ~a)"
                       delta x1 x2 vx vy)))))))

)

(define minha-janela (new equacao-frame%))
(send minha-janela show #t)
