main:
    li $a0 5
    sw $a0, 0($sp)
    addiu $sp, $sp, -4
    li $a0 2
    lw $t1, 4($sp)
    mul $a0, $t1, $a0
    addiu $sp, $sp, 4
    sw $a0, 0($sp)
    addiu $sp, $sp, -4
    li $a0 10
    lw $t1, 4($sp)
    div $a0, $t1, $a0
    addiu $sp, $sp, 4
    sw $a0, 0($sp)
    addiu $sp, $sp, -4
    li $a0 10
    lw $t1, 4($sp)
    mul $a0, $t1, $a0
    addiu $sp, $sp, 4
    sw $a0, 0($sp)
    addiu $sp, $sp, -4
    li $a0 5
    lw $t1, 4($sp)
    mul $a0, $t1, $a0
    addiu $sp, $sp, 4
    sw $a0, 0($sp)
    addiu $sp, $sp, -4
    li $a0 10
    lw $t1, 4($sp)
    mul $a0, $t1, $a0
    addiu $sp, $sp, 4
    sw $a0, 0($sp)
    addiu $sp, $sp, -4
    li $a0 5
    lw $t1, 4($sp)
    div $a0, $t1, $a0
    addiu $sp, $sp, 4
    sw $a0, 0($sp)
    addiu $sp, $sp, -4
    li $a0 2
    lw $t1, 4($sp)
    add $a0, $t1, $a0
    addiu $sp, $sp, 4
    sw $a0, 0($sp)
    addiu $sp, $sp, -4
    li $a0 4
    sw $a0, 0($sp)
    addiu $sp, $sp, -4
    li $a0 5
    lw $t1, 4($sp)
    mul $a0, $t1, $a0
    addiu $sp, $sp, 4
    sw $a0, 0($sp)
    addiu $sp, $sp, -4
    li $a0 6
    lw $t1, 4($sp)
    mul $a0, $t1, $a0
    addiu $sp, $sp, 4
    lw $t1, 4($sp)
    add $a0, $t1, $a0
    addiu $sp, $sp, 4