robotConstructor (name, attack, hp) = (\message -> message (name, attack, hp))

killerRobot = robotConstructor ("killer", 25, 200)
name (n, _, _) = n 
attack (_,a,_) = a
hp (_, _, hp) = hp

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHp aRobot = aRobot hp

setName aRobot newName = aRobot (\(_,a,h)-> robotConstructor (newName, a, h)) 
setAttack aRobot newAttack = aRobot (\ (n,_,h)-> robotConstructor (n, newAttack, h))
setHp aRobot newHp = aRobot (\(n,a,_) -> robotConstructor (n, a, newHp))

printRobot aRobot = aRobot (\(n,a,h) ->  n ++ " attack: " ++ show a ++ " hp: " ++ show h)

-- robotAttack firstRobot secondRobot = if firstRobotAttack >= secondRobot 
--     then setHp secondRobot 0
--     else setHp secondRobot (secondRobotHp - firstRobotAttack)
--     where firstRobotAttack = firstRobot (\(n,a,h) -> a ) 
--           secondRobotHp = secondRobot (\ (n,a,h) -> h)

damage aRobot attackDame = aRobot (\(n, a, h) -> robotConstructor (n, a, h - attackDame))

myFigth aRobot defender = if aRobotHp > 0 
    then damage defender aRobotAttack
    else damage defender 0
    where aRobotHp = getHp aRobot   
          aRobotAttack = getAttack aRobot

fight aRobot defender = damage defender attack
        where attack = if getHp aRobot > 10
                    then getAttack aRobot
                    else 0


fastRobot = robotConstructor ("fast", 15, 40)
slowRobot = robotConstructor ("slow", 20, 30)

fastRobotRound1 = fight slowRobot fastRobot
slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound2 = fight slowRobotRound1 fastRobotRound1
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
fastRobotRound3 = fight slowRobotRound2 fastRobotRound2
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2