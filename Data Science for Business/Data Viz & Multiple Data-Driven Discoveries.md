# Mutiple Data - Driven Discoveries

- if two factors are independent, then an level of one should have the same probability for each level of the other factor, e.g.
  P(Waiting|No Service) = P(Waiting|Service)
## Muptiple Testing and Discoveries
### Examples:
- Amazon recommendation system
- Music recommendation system
- Genome-wide association studies

If we proceed as usual, for example set α = 5, it will lead to high false discoveries rate 
since most likly only a tiny subset matters.

### Simultaneous testing 
|               | Applicable    | What is Needed| For regression| critical p-value |
| ------------- | ------------- | ------------- | ------------- | ------------- |
| Conservative  | Content Cell  | Content Cell  | Content Cell  | Content Cell  |
| Content Cell  | Content Cell  | Content Cell  | Content Cell  | Content Cell  |
| Content Cell  | Content Cell  | Content Cell  | Content Cell  | Content Cell  |
