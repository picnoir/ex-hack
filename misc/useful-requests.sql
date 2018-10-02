-- Those requests are quite handy to manually debug the software.

-- Check the packages dep graph
select p1.name, '=>', p2.name from dependancies d
inner join packages p1 on (d.packId = p1.packageId)
inner join packages p2 on (d.depId = p2.packageId);

-- Where did we find the occurences?
select p.name, em.name, es.name from symbolOccurences so
inner join exposedSymbols es on (so.importedSymId = es.id)
inner join exposedModules em on (es.modId = em.id)
inner join packages p on (em.packId = p.packageId);
